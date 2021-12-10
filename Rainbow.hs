{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Rainbow where

-- attempt to just do all graphics stuff in 1 file

import Control.Exception
import Data.Char
import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Codec.Picture

import Common
import Paint
import Event
import Glue
import HList


data PaintShop = PS
  { psSprite :: Paint SpriteUniforms
  , psPrism  :: Paint SpriteUniforms
  , psGlyph  :: Paint GlyphUniforms }

data GFX = GFX
  { gfxTile :: VBO
  , gfxPaints :: PaintShop
  , gfxFont :: Tex
  , gfxWindow :: GLFW.Window
  , gfxLogicalWindowDimensions :: IORef Float2
  , gfxPhysicalWindowDimensions :: IORef Int2 }
    

data Canvas = Canvas
  { canvasWH :: Int2
  , canvasTex :: Tex
  , canvasFbo :: FBO }

data GLFWException = GLFWException GLFW.Error String deriving Show
instance Exception GLFWException

data ImageException = ImageException String String deriving Show
instance Exception ImageException

-- load gfx stuff
rainbow winW winH scale var = do
  (win,physDims,logiDims) <- glfwRitual winW winH scale var
  tile <- loadBasicTile
  (font,_) <- loadTextureFromFile "cozette.png"
  spritePaint <- loadSpritePaint tile
  prismPaint <- loadPrismPaint tile
  glyphPaint <- loadGlyphPaint tile

  let gfx = GFX{
    gfxTile=tile,
    gfxPaints=PS{
      psSprite=spritePaint,
      psPrism=prismPaint,
      psGlyph=glyphPaint
    },
    gfxFont=font,
    gfxWindow=win,
    gfxLogicalWindowDimensions=logiDims,
    gfxPhysicalWindowDimensions=physDims
  }

  return (win, gfx)


newCanvas :: Int -> Int -> IO Canvas
newCanvas w h = do
  fbo <- newFBO
  useFBO fbo
  surf <- newBlankTexture w h
  useTexture surf
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  attachTex surf
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  if status /= GL_FRAMEBUFFER_COMPLETE
    then putStrLn "framebuffer incomplete"
    else return ()
  useFBO (FBO 0)
  return Canvas{canvasWH=I2 w h, canvasTex=surf, canvasFbo=fbo}

blitCanvas :: GFX -> Canvas -> Float2 -> IO ()
blitCanvas gfx cnv (F2 x y) = do
  let Paint vao shader uniforms = (psPrism . gfxPaints) gfx
  let tile = gfxTile gfx
  useVAO vao
  useVBO tile
  useShader shader
  useTexture (canvasTex cnv)
  let I2 w h = canvasWH cnv
  d <- readIORef (gfxLogicalWindowDimensions gfx)
  configUniforms uniforms $
    Field @"winWH" d >:
    Field @"srcXYWH" (F4 0 0 (fi w) (fi h)) >:
    Field @"dstXYWH" (F4 x y (fi w) (fi h)) >:
    R0
  renderQuad

loadBasicTile :: IO VBO
loadBasicTile = storableVectorToVBO tileData

loadTextureFromFile :: FilePath -> IO (Tex, Int2)
loadTextureFromFile path = do
  tex <- readImage path >>= \x -> case x of
    Left msg -> throwIO (ImageException path msg)
    Right di -> pictureToTex di
  assertGL "load image file"
  return tex

type SpriteUniforms =
  ['("winWH",   Float2)
  ,'("srcXYWH", Float4)
  ,'("dstXYWH", Float4)]

type GlyphUniforms =
  ['("winWH",   Float2)
  ,'("srcXYWH", Float4)
  ,'("dstXYWH", Float4)
  ,'("fgColor", Float3)
  ,'("bgColor", Float3)]

loadSpritePaint :: VBO -> IO (Paint SpriteUniforms)
loadSpritePaint vbo = do
  vao <- newVAO
  useVAO vao
  let path1 = "shaders/sprite/vertex.glsl"
  let path2 = "shaders/sprite/fragment.glsl"
  code1 <- readFile path1
  code2 <- readFile path2
  shader <- newShader path1 code1 path2 code2
  useVBO vbo
  configAttrib shader "position" 2 8 0 GL_FLOAT

  let stones = ug2f . ug4f . ug4f
  gate <- buildGate shader (stones capstone)

  return (Paint vao shader gate)

loadPrismPaint :: VBO -> IO (Paint SpriteUniforms)
loadPrismPaint vbo = do
  vao <- newVAO
  useVAO vao
  let path1 = "shaders/prism/vertex.glsl"
  let path2 = "shaders/prism/fragment.glsl"
  code1 <- readFile path1
  code2 <- readFile path2
  shader <- newShader path1 code1 path2 code2
  useVBO vbo
  configAttrib shader "position" 2 8 0 GL_FLOAT

  let stones = ug2f . ug4f . ug4f
  gate <- buildGate shader (stones capstone)

  return (Paint vao shader gate)

loadGlyphPaint :: VBO -> IO (Paint GlyphUniforms)
loadGlyphPaint vbo = do
  vao <- newVAO
  useVAO vao
  let path1 = "shaders/glyph/vertex.glsl"
  let path2 = "shaders/glyph/fragment.glsl"
  code1 <- readFile path1
  code2 <- readFile path2
  shader <- newShader path1 code1 path2 code2
  useVBO vbo
  configAttrib shader "position" 2 8 0 GL_FLOAT

  let stones = ug2f . ug4f . ug4f . ug3f . ug3f
  gate <- buildGate shader (stones capstone)

  return (Paint vao shader gate)


type Torch1 = Char -> Float3 -> Float3 -> Int2 -> IO ()
withFont :: GFX -> Canvas -> (Torch1 -> IO ()) -> IO ()
withFont gfx cnv action = do
  let Paint vao shader uniforms = (psGlyph . gfxPaints) gfx
  useVAO vao
  useVBO (gfxTile gfx)
  useShader shader
  useTexture (gfxFont gfx) -- note, this is the font not the drawing surface
  --glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  --glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  useFBO (canvasFbo cnv)
  let d = i22f2 (canvasWH cnv)
  let I2 cw ch = canvasWH cnv
  glViewport 0 0 (fi cw) (fi ch)

  action $ \c fg bg (I2 i j) -> do
    let glyph = ord c - 33
    let w = 7
    let h = 13
    let fw = fi w
    let fh = fi h
    let sx = fi $ 2 * w * (glyph `mod` 40) + 42
    let sy = fi $ h * (glyph `div` 40) + 42
    let dx = fi (i * w) - (fi cw / 2) + 2
    let dy = fi (j * h) - (fi ch / 2) + 8
    configUniforms uniforms $
      Field @"winWH" d >:
      Field @"srcXYWH" (F4 sx sy fw fh) >:
      Field @"dstXYWH" (F4 dx dy fw fh) >:
      Field @"fgColor" fg >:
      Field @"bgColor" bg >:
      R0
    renderQuad
  I2 w h <- readIORef (gfxPhysicalWindowDimensions gfx)
  glViewport 0 0 (fi w) (fi h)
  useFBO (FBO 0)
  assertGL "withFont"


type Torch2 = Int -> Float2 -> IO ()
withSprites :: GFX -> Tex -> Canvas -> (Torch2 -> IO ()) -> IO ()
withSprites gfx sheet cnv action = do
  let Paint vao shader uniforms = (psSprite . gfxPaints) gfx
  useVAO vao
  useVBO (gfxTile gfx)
  useShader shader
  useTexture sheet
  useFBO (canvasFbo cnv)
  let d = i22f2 (canvasWH cnv)
  let I2 cw ch = canvasWH cnv
  glViewport 0 0 (fi cw) (fi ch)

  action $ \spriteNo (F2 x y) -> do
    let w = 32
    let h = 32
    let fw = fi w
    let fh = fi h
    let (sj, si) = divMod spriteNo 13
    let sx = fi $ 32 * si
    let sy = fi $ 32 * sj
    configUniforms uniforms $
      Field @"winWH" d >:
      Field @"srcXYWH" (F4 sx sy fw fh) >:
      Field @"dstXYWH" (F4 x y fw fh) >:
      R0
    renderQuad
  I2 w h <- readIORef (gfxPhysicalWindowDimensions gfx)
  glViewport 0 0 (fi w) (fi h)
  useFBO (FBO 0)
  assertGL "withSprites"


-- takes logical window size
glfwRitual w h scale var = do
  GLFW.setErrorCallback $ Just $ \err msg -> do
    GLFW.terminate
    throwIO (GLFWException err msg)

  GLFW.init

  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)

  Just win <- GLFW.createWindow (scale * w) (scale * h) ":D" Nothing Nothing 
  GLFW.makeContextCurrent (Just win)

  cullBackFaces -- debug only

  physDims <- newIORef $ I2 (scale * w) (scale * h)
  logiDims <- newIORef $ F2 (fi w) (fi h)

  let push :: Event -> IO ()
      push e = do
        modifyIORef' var (e:)

  let r2f = realToFrac
  GLFW.setWindowSizeCallback win $ Just $ \win w h -> do
    print ("window resize", w, h)
    push (WinSize w h)
  GLFW.setFramebufferSizeCallback win $ Just $ \win w h -> do
    print ("fb resize", w, h)
    writeIORef physDims (I2 w h)
    glViewport 0 0 (fi w) (fi h)
  GLFW.setKeyCallback win $ Just $ \win key n state mods -> do
    push (translateGLFWKey key state mods)
  GLFW.setCharCallback win $ Just $ \win c -> push (Typing c)
  GLFW.setMouseButtonCallback win $ Just $ \win button state mods -> case state of
    GLFW.MouseButtonState'Pressed  -> push (MouseClick (fromEnum button))
    GLFW.MouseButtonState'Released -> push (MouseUnclick (fromEnum button))
  GLFW.setCursorPosCallback win $ Just $ \win x y -> push (MouseMotion (r2f x) (r2f y))
  GLFW.setScrollCallback win $ Just $ \win dx dy -> push (Scroll (r2f dx) (r2f dy))

  return (win, physDims, logiDims)
