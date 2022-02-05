{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module OneShot where

import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as VS
--import qualified Data.Vector.Storable.Mutable as VS (write)
import Codec.Picture (DynamicImage)
import Data.Proxy
import Control.Monad
import Data.Foldable
import Graphics.GL
import GHC.TypeLits
import Codec.Picture as JuicyPixels
import Control.Exception

import Common
import Glue
import HList
import Paint


import Shape
import ShapeCompiler

-- unif, the names and types of uniforms as a type level list of pairs
-- a, the vertex type which should be an instead of ATTRIBS.
--
-- geometry is a set of primitives made of a-vertexes
-- program is a shader that expects geometry in a-vertex format and wants you to set uniforms
data OneShot a u = OneShot
  { oneGeom  :: Prims a
  , oneImag  :: [TexImage]
  , oneOpts  :: [Option]
  , oneProg  :: ProgSrc a u
  , oneUnis  :: Rec u }
      deriving Show

data Prims a
  = TriangleList  [Tri a]
  | TriangleFan   [a]
  | TriangleStrip [a]
      deriving (Show, Functor, Foldable)

data TexImage = TexImage ImgSrc TexParams

data ImgSrc
  = LibraryImg String
  | forall a u . (Show a, ShowRec u, ATTRIBS a, Uniforms u) => GPUImg (Int, Int, OneShot a u)

deriving instance Show ImgSrc

data Option = Blending Bool deriving (Show)

primProxy :: Prims a -> Proxy a
primProxy _ = Proxy

data Tri a = Tri !a !a !a deriving (Show, Functor, Foldable)

marsh :: ATTRIBS a => Prims a -> VS.Vector Float
marsh (TriangleFan xs) = VS.fromList (concatMap marshal xs)
marsh (TriangleStrip xs) = VS.fromList (concatMap marshal xs)
marsh (TriangleList ts) = VS.fromList (concatMap (concatMap marshal . toList) ts)

data ProgSrc a u = ProgSrc String String deriving Show

instance Show TexImage where
  show _ = "TexImage _ _"

data SomeAttrib = SomeAttrib String Int

class ATTRIBS a where
  spellItOut :: Proxy a -> [SomeAttrib]
  marshal    :: a -> [Float]

data MyVertex = MyVertex
  { position :: Float2 } deriving Show

instance ATTRIBS MyVertex where
  spellItOut _ = [SomeAttrib "position" 2]
  marshal (MyVertex (F2 x y)) = [x,y]

configureTheAttribs :: ATTRIBS a => Proxy a -> Shader -> IO ()
configureTheAttribs prox shader = do
  let attrs = spellItOut prox
  let getN (SomeAttrib _ n) = n
  let stride = sum (map getN attrs) * 4
  let offsets = scanl (\off (SomeAttrib _ n) -> off + n) 0 attrs
  forM_ (zip attrs offsets) $ \(SomeAttrib name noComp, off) -> do
    configAttrib shader name noComp stride off GL_FLOAT
    


-- fucking vertex attribs!
--
-- the geometry we give to the one shot contains a bunch of vertices.
-- we have to 1. upload the data, which means flattening out all the data into a buffer of floats easy.
-- 2. execute configAttrib(shader, name, nocomponents, attrib size, offset, data type) for each component
--
-- The details necessary to call all these configAttribs are encoded in the type of the vertex format.
-- So we want system of types for vertex formats.



splat
  :: Sh Color
  -> XYWH     -- destination xy, cropping window size in screen coords
  -> Float    -- scale factor, 1 = leave tiny
  -> Int2     -- canvas dimensions in screen coords
  -> IO ()
splat s xywh scale (Common.I2 cw ch) = go where
  frag = toFrag s
  geom = TriangleFan
          (map MyVertex
            [F2 -1 -1
            ,F2  1 -1
            ,F2  1  1
            ,F2 -1  1])
  unis = Field @"outScale" scale >:
         Field @"outXYWH" xywh >:
         Field @"canvasWH" (F2 (fromIntegral cw) (fromIntegral ch)) >:
         R0
  vert =
    (unlines
    ["#version 150"

    ,"in vec2 position;"
    ,"uniform vec4 outXYWH;" -- screen coords, 0 0 is middle of frame buffer
    ,"uniform float outScale;" -- 1 = don't scale, but will be quite small (~ 1 pixel)
    ,"uniform vec2 canvasWH;" -- dimensions of screen in screen coords
    ,"out vec2 uv;"
    ,"out vec2 pixelWH;" -- in uv coords, pixels should always fit nicely in the outbox

    ,"void main(){"
    ,"  float x = outXYWH.x;"
    ,"  float y = outXYWH.y;"
    ,"  float w = outXYWH.z;"
    ,"  float h = outXYWH.w;"
    ,"  pixelWH = vec2(1/outScale,1/outScale);"
    ,"  uv = position * vec2(w/2, h/2) / outScale;"
    ,"  vec2 xy = position * vec2(w,h)/canvasWH + 2*vec2(x,y)/canvasWH;"
    ,"  gl_Position = vec4(xy, 0, 1);"
    ,"}"
    ])

  shot = 
    OneShot {
      oneGeom = geom,
      oneUnis = unis,
      oneImag = [],
      oneOpts = [Blending False],
      oneProg = ProgSrc vert frag
    }

  alertMsg = "no samplers expected when splatting a color picture"

  go = renderOneShot (const (error alertMsg)) shot

-- *** RESUME WORK HERE, NEED ALPHA BLENDING OPTION FOR OneShot ***
splatMask
  :: Sh (Maybe Color)
  -> XYWH     -- destination xy, cropping window size in screen coords
  -> Float    -- scale factor, 1 = leave tiny
  -> Int2     -- canvas dimensions in screen coords
  -> IO ()
splatMask s xywh scale (Common.I2 cw ch) = go where
  frag = toFragMask s
  geom = TriangleFan
          (map MyVertex
            [F2 -1 -1
            ,F2  1 -1
            ,F2  1  1
            ,F2 -1  1])
  unis = Field @"outScale" scale >:
         Field @"outXYWH" xywh >:
         Field @"canvasWH" (F2 (fromIntegral cw) (fromIntegral ch)) >:
         R0
  vert =
    (unlines
    ["#version 150"

    ,"in vec2 position;"
    ,"uniform vec4 outXYWH;" -- screen coords, 0 0 is middle of frame buffer
    ,"uniform float outScale;" -- 1 = don't scale, but will be quite small (~ 1 pixel)
    ,"uniform vec2 canvasWH;" -- dimensions of screen in screen coords
    ,"out vec2 uv;"
    ,"out vec2 pixelWH;" -- in uv coords, pixels should always fit nicely in the outbox

    ,"void main(){"
    ,"  float x = outXYWH.x;"
    ,"  float y = outXYWH.y;"
    ,"  float w = outXYWH.z;"
    ,"  float h = outXYWH.w;"
    ,"  pixelWH = vec2(1/outScale,1/outScale);"
    ,"  uv = position * vec2(w/2, h/2) / outScale;"
    ,"  vec2 xy = position * vec2(w,h)/canvasWH + 2*vec2(x,y)/canvasWH;"
    ,"  gl_Position = vec4(xy, 0, 1);"
    ,"}"
    ])

  shot = 
    OneShot {
      oneGeom = geom,
      oneUnis = unis,
      oneImag = [],
      oneOpts = [Blending True],
      oneProg = ProgSrc vert frag
    }

  alertMsg = "no samplers expected when splatting a color picture"

  go = renderOneShot (const (error alertMsg)) shot
  


-- | 1. create a vertex buffer for all the a, fill the buffer, upload it
-- 2. create a VAO 
-- 3. compile link upload the program
-- 4. configure VAO against buffer and program, using attribs
-- 5. lookup uniform locations
-- 6. if image present, upload to get texture, use texture
-- 7. set uniforms using provided parameters
-- 8. don't worry about FBOs right now
-- 9. issue the render command, drawTriangles or such
-- 10. destroy all objects we just created.
renderOneShot
  :: (ATTRIBS a, Uniforms u)
  => (String -> DynamicImage)
  -> OneShot a u
  -> IO ()
renderOneShot imglib (OneShot geo teximgs opts (ProgSrc vsrc fsrc) univals) = do

  -- Gather any images
  imgs <- forM teximgs $ \(TexImage ti _) -> case ti of
    GPUImg (w,h,shot) -> imageOneShot imglib w h shot
    LibraryImg name -> return (imglib name)

  -- VAO saves the attribs configured between shader and buffer
  vao <- newVAO
  useVAO vao

  -- Compile a shader, query the uniform locations by name
  shader <- newShader "vsrc" vsrc "fsrc" fsrc
  uniformLocs <- getLocs shader
  useShader shader

  -- Marshal geometry into a vector, then upload to VBO
  let geovec = marsh geo
  let n = fromIntegral (VS.length geovec)
  vbo <- storableVectorToVBO geovec
  useVBO vbo

  -- CONFIGURE THE ATTRIBS
  configureTheAttribs (primProxy geo) shader

  -- Upload images to textures and bind them to texture units
  texes <- forM imgs $ \img -> do
    (tex, _) <- pictureToTex img
    return tex

  forM_ (zip [0..] texes) $ \(i,tex) -> do
    useTextureN tex i

  -- Set all the uniforms
  setUniforms uniformLocs univals shader

  forM_ opts $ \o -> case o of
    Blending True -> do
      glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      glEnable GL_BLEND
    Blending False -> do
      glDisable GL_BLEND

  -- Render command
  --glViewport 0 0 10 10
  case geo of
    TriangleList _  -> glDrawArrays GL_TRIANGLES 0 n
    TriangleFan _   -> glDrawArrays GL_TRIANGLE_FAN 0 n
    TriangleStrip _ -> glDrawArrays GL_TRIANGLE_STRIP 0 n

  -- Delete it all
  deleteTextures texes
  deleteVBO vbo
  deleteShader shader
  deleteVAO vao


imageOneShot
  :: (ATTRIBS a, Uniforms u)
  => (String -> DynamicImage)
  -> Int
  -> Int
  -> OneShot a u
  -> IO DynamicImage
imageOneShot imglib w h (OneShot geo teximgs _ (ProgSrc vsrc fsrc) univals) = do

  -- Gather images, which might mean rendering something else first.
  imgs <- forM teximgs $ \(TexImage ti _) -> case ti of
    GPUImg (w,h,shot) -> imageOneShot imglib w h shot
    LibraryImg name -> return (imglib name)

  -- FBO for recieving the image of desired size
  fbo <- newFBO
  useFBO fbo
  surf <- newBlankTexture w h
  useTexture surf
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  attachTex surf
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  if status /= GL_FRAMEBUFFER_COMPLETE
    then throwIO (userError "framebuffer incomplete")
    else return ()
  useFBO (FBO 0)

  -- VAO saves the attribs configured between shader and buffer
  vao <- newVAO
  useVAO vao

  -- Compile a shader, query the uniform locations by name
  shader <- newShader "vsrc" vsrc "fsrc" fsrc
  uniformLocs <- getLocs shader
  useShader shader

  -- Marshal geometry into a vector, then upload to VBO
  let geovec = marsh geo
  let n = fromIntegral (VS.length geovec)
  vbo <- storableVectorToVBO geovec
  useVBO vbo

  -- CONFIGURE THE ATTRIBS
  configureTheAttribs (primProxy geo) shader

  -- Upload images to textures and bind them to texture units
  texes <- forM imgs $ \img -> do
    (tex, _) <- pictureToTex img
    return tex

  forM_ (zip [0..] texes) $ \(i,tex) -> do
    useTextureN tex i

  -- Set all the uniforms
  setUniforms uniformLocs univals shader


  -- Render command
  useFBO fbo
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  case geo of
    TriangleList _  -> glDrawArrays GL_TRIANGLES 0 n
    TriangleFan _   -> glDrawArrays GL_TRIANGLE_FAN 0 n
    TriangleStrip _ -> glDrawArrays GL_TRIANGLE_STRIP 0 n
  -- Download the image data

  raw <- dumpFramebufferRGBA 0 0 w h -- Vector Word8, format RGB8
  let imgOut = Image w h raw
  useFBO (FBO 0)

  -- Delete it all
  deleteTexture surf
  deleteFBO fbo
  -- DELETE TARGET TEX or RBO
  -- DELETE FBO
  deleteTextures texes
  deleteVBO vbo
  deleteShader shader
  deleteVAO vao

  return (ImageRGBA8 imgOut)

loadExample = do
  Right img1 <- readImage "item.png"
  Right img2 <- readImage "yoshi.png"
  return (img1,img2)
  --return (example [TexImage img1 id, TexImage img2 id])

example :: [TexImage]
        -> OneShot MyVertex '[ '("color",Float3)
                             , '("sprite1",Sampler2D)
                             , '("sprite2",Sampler2D) ]
example imgs = OneShot
  { oneGeom = TriangleFan
                (map MyVertex
                  [F2 -1 -1
                  ,F2  1 -1
                  ,F2  1  1
                  ,F2 -1  1])
  , oneUnis = Field @"color"   (F3 1 0 0) >:
              Field @"sprite1" (Sampler2D 0) >:
              Field @"sprite2" (Sampler2D 1) >:
              R0
  , oneImag = imgs
  , oneOpts = mempty
  , oneProg = ProgSrc

(unlines
["#version 150"

,"in vec2 position;"
,"out vec2 uv;"

,"void main(){"
,"  uv = position;"
,"  gl_Position = vec4(position * 1, 0, 1);"
,"}"
])

(toFrag (BGColor (C 1 1 1) (Colorize (C 0 0 0) bdata)))
{-
(toFrag (BGColor (C 0 0 0)
  (Colorize (C 0 1 0)
    (Minus
      (Shift (F2 0.5 0) Ball)
      (Curve2O (F2 -0.5 -0.5) (F2 0 0.5) (F2 0.5 -0.5))
    ))))
-}
    }


-- the raw shape data structure can be compiled into a "constant" shader
-- when uploaded displays the shape and the frame buffer can be dumped to disk.

-- 



