module Glue where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import System.Exit
import Foreign.Marshal
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Control.Monad (when)
import Data.Word

import Graphics.GL
import Codec.Picture

import Common

newtype VAO = VAO GLuint
newtype VBO = VBO GLuint
newtype Shader = Shader GLuint
newtype Tex = Tex GLuint
newtype FBO = FBO GLuint

newtype UL = UL GLint -- uniform location

-- VAO
newVAO :: IO VAO
newVAO = do
  fmap VAO $ alloca (\ptr -> glGenVertexArrays 1 ptr >> peek ptr)

useVAO :: VAO -> IO ()
useVAO (VAO i) = glBindVertexArray i

-- VBO
newVBO :: Ptr a -> Int -> IO VBO
newVBO ptr n = do
  i <- alloca (\ptr -> glGenBuffers 1 ptr >> peek ptr)
  glBindBuffer GL_ARRAY_BUFFER i
  glBufferData GL_ARRAY_BUFFER (fromIntegral n) (castPtr ptr) GL_STATIC_DRAW
  return (VBO i)

useVBO :: VBO -> IO ()
useVBO (VBO n) = glBindBuffer GL_ARRAY_BUFFER n

storableVectorToVBO :: V.Vector Float -> IO VBO
storableVectorToVBO v = do
  let (fptr, len) = V.unsafeToForeignPtr0 v
  let s = sizeOf (undefined :: Float)
  withForeignPtr fptr $ \ptr -> newVBO ptr (len * s)

-- shader
newShader :: String -> String -> String -> String -> IO Shader
newShader vsname vscode fsname fscode = do
  vs <- compileShader vsname vscode GL_VERTEX_SHADER
  fs <- compileShader fscode fscode GL_FRAGMENT_SHADER
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
pictureToTex :: DynamicImage -> IO Tex
pictureToTex di = action where
  action = case di of
    ImageRGBA8 (Image w h pixVec) -> do
      V.unsafeWith pixVec $ \ptr -> do
        newTexture GL_RGBA ptr w h
    ImageRGB8 (Image w h pixVec) -> do
      V.unsafeWith pixVec $ \ptr -> do
        newTexture GL_RGB ptr w h
    _ -> do
      putStrLn "unsupported format"
      exitFailure

newTexture :: GLenum -> Ptr Word8 -> Int -> Int -> IO Tex
newTexture format ptr w h = do
  n <- alloca (\p -> glGenTextures 1 p >> peek p)
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

setUniform2f :: GLint -> Float -> Float -> IO ()
setUniform2f ul x y =
  withArray [x,y] $ \ptr -> glUniform2fv ul 1 ptr

setUniform3f :: GLint -> Float -> Float -> Float -> IO ()
setUniform3f ul x y z =
  withArray [x,y,z] $ \ptr -> glUniform3fv ul 1 ptr


-- rendering
renderQuad :: IO ()
renderQuad = do
  glDrawArrays GL_TRIANGLES 0 6

clearColorBuffer :: IO ()
clearColorBuffer = do
  glClearColor 1 0 0 1
  glClear GL_COLOR_BUFFER_BIT

clearWithGreen :: IO ()
clearWithGreen = do
  glClearColor 0 1 0 1
  glClear GL_COLOR_BUFFER_BIT

cullBackFaces :: IO ()
cullBackFaces = do
  glCullFace GL_BACK 
  glEnable GL_CULL_FACE


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
