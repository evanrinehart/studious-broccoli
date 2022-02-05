{-# LANGUAGE TypeApplications #-}
module Glue where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM (new, unsafeWith)
import System.Exit
import Foreign.Marshal
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Control.Monad (when)
import Control.Exception
import Data.Word
import Data.Int

import Graphics.GL
import Codec.Picture

import Common

newtype VAO = VAO GLuint
newtype VBO = VBO GLuint
newtype Shader = Shader GLuint
newtype Tex = Tex GLuint
newtype FBO = FBO GLuint

newtype UL = UL GLint -- uniform location

type Slapper = Rect Float -> Tex -> IO ()


data WrapMode = WrapClampToEdge | WrapRepeat | WrapMirroredRepeat deriving Show
data TexFilter = NearestFilter | LinearFilter deriving Show
data TexParams = TexParams
  { texWrapS   :: WrapMode
  , texWrapT   :: WrapMode
  , texMinFilt :: TexFilter
  , texMagFilt :: TexFilter } deriving Show

texWrap   mode tp = tp { texWrapS = mode, texWrapT = mode }
texFilter mode tp = tp { texMinFilt = mode, texMagFilt = mode }
texParamDefaults = TexParams WrapRepeat WrapRepeat LinearFilter LinearFilter



-- VAO
newVAO :: IO VAO
newVAO = do
  fmap VAO $ alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)

useVAO :: VAO -> IO ()
useVAO (VAO i) = glBindVertexArray i

-- VBO
newVBO :: Ptr a -> Int -> IO VBO
newVBO src n = do
  i <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer GL_ARRAY_BUFFER i
  glBufferData GL_ARRAY_BUFFER (fromIntegral n) (castPtr src) GL_STATIC_DRAW
  return (VBO i)

useVBO :: VBO -> IO ()
useVBO (VBO n) = glBindBuffer GL_ARRAY_BUFFER n

storableVectorToVBO :: VS.Vector Float -> IO VBO
storableVectorToVBO v = do
  let (fptr, len) = VS.unsafeToForeignPtr0 v
  let s = sizeOf (undefined :: Float)
  withForeignPtr fptr $ \ptr -> newVBO ptr (len * s)

-- shader
newShader :: String -> String -> String -> String -> IO Shader
newShader vsname vscode fsname fscode = do
  vs <- compileShader vsname vscode GL_VERTEX_SHADER
  fs <- compileShader fsname fscode GL_FRAGMENT_SHADER
  pro <- glCreateProgram
  glAttachShader pro vs
  glAttachShader pro fs
  glLinkProgram pro
  whenLinkFailed pro $ \errors -> do
    putStrLn vsname
    putStrLn fsname
    mapM_ putStrLn errors
    exitFailure
  glDeleteShader vs
  glDeleteShader fs
  return (Shader pro)

compileShader :: String -> String -> GLuint -> IO GLuint
compileShader filename code shaderType = do
  shaderId <- glCreateShader shaderType
  withCString code $ \ptr -> with ptr $ \pptr -> do
    glShaderSource shaderId 1 pptr nullPtr
    glCompileShader shaderId
  whenCompileFailed shaderId $ \errors -> do
    putStrLn filename
    mapM_ putStrLn errors
    exitFailure
  return shaderId

whenCompileFailed :: GLuint -> ([String] -> IO ()) -> IO ()
whenCompileFailed shaderId action = do
  result <- with GL_FALSE $ \ptr -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS ptr
    peek ptr
  when (result == GL_FALSE) $ do
    len <- fmap fromIntegral $ alloca $ \ptr ->
      glGetShaderiv shaderId GL_INFO_LOG_LENGTH ptr >> peek ptr
    errors <- allocaArray len $ \ptr -> do
      glGetShaderInfoLog shaderId (fromIntegral len) nullPtr ptr
      peekCString ptr
    action (lines errors)

whenLinkFailed :: GLuint -> ([String] -> IO ()) -> IO ()
whenLinkFailed programId action = do
  result <- alloca (\ptr -> glGetProgramiv programId GL_LINK_STATUS ptr >> peek ptr)
  when (result == GL_FALSE) $ do
    len <- fmap fromIntegral $ alloca $ \ptr -> do
      glGetProgramiv programId GL_INFO_LOG_LENGTH ptr
      peek ptr
    errors <- allocaArray len $ \ptr -> do
      glGetProgramInfoLog programId (fromIntegral len) nullPtr ptr
      peekCString ptr
    action (lines errors)

useShader :: Shader -> IO ()
useShader (Shader i) = glUseProgram i


-- Texture
pictureToTex :: DynamicImage -> IO (Tex,Int2)
pictureToTex di = action where
  action = case di of
    ImageRGBA8 (Image w h pixVec) -> do
      VS.unsafeWith pixVec $ \ptr -> do
        tex <- newTexture GL_RGBA ptr w h
        return (tex, I2 w h)
    ImageRGB8 (Image w h pixVec) -> do
      VS.unsafeWith pixVec $ \ptr -> do
        tex <- newTexture GL_RGB ptr w h
        return (tex, I2 w h)
    _ -> do
      putStrLn "unsupported format"
      exitFailure

newTexture :: GLenum -> Ptr Word8 -> Int -> Int -> IO Tex
newTexture format ptr w h = do
  n <- alloca (\p -> glGenTextures 1 p >> peek p)
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D n
  glTexImage2D
    GL_TEXTURE_2D
    0 -- level of detail number
    GL_RGBA
    (fromIntegral w)
    (fromIntegral h)
    0 -- border (0 or 1)
    format -- format of image data
    GL_UNSIGNED_BYTE -- image data type 
    (castPtr ptr)
  glGenerateMipmap GL_TEXTURE_2D
  --glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  --glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  return (Tex n)

newBlankTexture :: Int -> Int -> IO Tex
newBlankTexture w h = do
  n <- alloca (\p -> glGenTextures 1 p >> peek p)
  glBindTexture GL_TEXTURE_2D n
  glTexImage2D
    GL_TEXTURE_2D
    0 -- level of detail number
    GL_RGBA
    (fromIntegral w)
    (fromIntegral h)
    0 -- border (0 or 1)
    GL_RGBA
    GL_UNSIGNED_BYTE -- image data type 
    nullPtr
  return (Tex n)

useTexture :: Tex -> IO ()
useTexture (Tex n) = glBindTexture GL_TEXTURE_2D n

useTextureN :: Tex -> Int -> IO ()
useTextureN (Tex n) unitNo = do
  case unitNo of
    0 -> glActiveTexture GL_TEXTURE0
    1 -> glActiveTexture GL_TEXTURE1
    2 -> glActiveTexture GL_TEXTURE2
    3 -> glActiveTexture GL_TEXTURE3
    4 -> glActiveTexture GL_TEXTURE4
    5 -> glActiveTexture GL_TEXTURE5
    6 -> glActiveTexture GL_TEXTURE6
    7 -> glActiveTexture GL_TEXTURE7
    _ -> throwIO (userError ("useTextureN _ " ++ show unitNo))
  glBindTexture GL_TEXTURE_2D n
  glActiveTexture GL_TEXTURE0
  

-- FBO
newFBO :: IO FBO
newFBO = do
  n <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)
  return (FBO n)

useFBO :: FBO -> IO ()
useFBO (FBO i) = glBindFramebuffer GL_FRAMEBUFFER i

attachTex :: Tex -> IO ()
attachTex (Tex i) =
  glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D i 0

-- attribs and uniforms
-- p - shader
-- name - attrib name
-- components - n components
-- stride - bytes per vertex
-- offset - byte offset of this attrib
-- dataType - type of component (int, float, etc)
configAttrib :: Shader -> String -> Int -> Int -> Int -> GLenum -> IO ()
configAttrib (Shader p) name components stride offset dataType = do
  attrib <- withCString name $ \ptr -> glGetAttribLocation p (castPtr ptr)
  glVertexAttribPointer
    (fi attrib)
    (fi components)
    dataType
    GL_FALSE
    (fi stride)
    (castPtr (nullPtr `plusPtr` offset))
  glEnableVertexAttribArray (fi attrib)

getUniformLocation :: Shader -> String -> IO GLint
getUniformLocation (Shader pr) name =
  withCString name (\ptr -> glGetUniformLocation pr ptr)

setUniform1i :: GLint -> Int32 -> IO ()
setUniform1i ul i = glUniform1i ul i

setUniform1f :: GLint -> Float -> IO ()
setUniform1f ul x = glUniform1f ul x

setUniform2f :: GLint -> Float -> Float -> IO ()
setUniform2f ul x y =
  withArray [x,y] $ \ptr -> glUniform2fv ul 1 ptr

setUniform3f :: GLint -> Float -> Float -> Float -> IO ()
setUniform3f ul x y z =
  withArray [x,y,z] $ \ptr -> glUniform3fv ul 1 ptr

setUniform4f :: GLint -> Float -> Float -> Float -> Float -> IO ()
setUniform4f ul x y z w =
  withArray [x,y,z,w] $ \ptr -> glUniform4fv ul 1 ptr

-- object disposal
deleteTexture :: Tex -> IO ()
deleteTexture (Tex n) = do
  withArray [n] $ \ptr -> glDeleteTextures 1 ptr

deleteTextures :: [Tex] -> IO ()
deleteTextures ts = do
  let ns = map (\(Tex n) -> n) ts
  withArray ns $ \ptr -> glDeleteTextures (fromIntegral (length ts)) ptr

deleteVBO :: VBO -> IO ()
deleteVBO (VBO i) = do
  withArray [i] $ \ptr -> glDeleteBuffers 1 ptr

deleteVAO :: VAO -> IO ()
deleteVAO (VAO i) = do
  withArray [i] $ \ptr -> glDeleteVertexArrays 1 ptr

deleteShader :: Shader -> IO ()
deleteShader (Shader i) = glDeleteProgram i

deleteFBO :: FBO -> IO ()
deleteFBO (FBO n) = do
  withArray [n] $ \ptr -> glDeleteFramebuffers 1 ptr
  

-- rendering
renderQuad :: IO ()
renderQuad = do
  glDrawArrays GL_TRIANGLES 0 6

clearColorBuffer :: Float -> Float -> Float -> IO ()
clearColorBuffer r g b = do
  glClearColor r g b 1
  glClear GL_COLOR_BUFFER_BIT

cullBackFaces :: IO ()
cullBackFaces = do
  glCullFace GL_BACK 
  glEnable GL_CULL_FACE


-- downloading
{-
void glReadPixels(
  GLint x,
	GLint y,
	GLsizei width,
	GLsizei height,
	GLenum format,
	GLenum type,
	GLvoid * data);
-}

dumpFramebufferRGB :: Int -> Int -> Int -> Int -> IO (VS.Vector Word8)
dumpFramebufferRGB x y w h = do
  mbuf <- VSM.new (w * h * 3)
  let fi = fromIntegral
  VSM.unsafeWith mbuf $ \ptr -> do
    glReadPixels (fi x) (fi y) (fi w) (fi h) GL_RGB GL_UNSIGNED_BYTE (castPtr ptr)
  assertGL "dumpFramebufferRGB:"
  buf <- VS.freeze mbuf
  return buf

dumpFramebufferRGBA :: Int -> Int -> Int -> Int -> IO (VS.Vector Word8)
dumpFramebufferRGBA x y w h = do
  mbuf <- VSM.new (w * h * 4)
  let fi = fromIntegral
  VSM.unsafeWith mbuf $ \ptr -> do
    glReadPixels (fi x) (fi y) (fi w) (fi h) GL_RGBA GL_UNSIGNED_BYTE (castPtr ptr)
  assertGL "dumpFramebufferRGBA:"
  buf <- VS.freeze mbuf
  return buf

-- errors
getError :: IO (Maybe String)
getError = do
  n <- glGetError
  return $ case n of
    GL_NO_ERROR -> Nothing
    GL_INVALID_ENUM -> Just "glError: enum"
    GL_INVALID_VALUE -> Just "glError: value"
    GL_INVALID_OPERATION -> Just "glError: operation"
    GL_INVALID_FRAMEBUFFER_OPERATION -> Just "glError: framebuffer operation"
    GL_OUT_OF_MEMORY -> Just "glError memory"
    _ -> Just "glError unknown"

assertGL :: String -> IO ()
assertGL hint = getError >>= \x -> case x of
  Just msg -> do
    putStrLn ("@" ++ hint ++ " " ++ msg)
    exitFailure
  Nothing -> return ()
