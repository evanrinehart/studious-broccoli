{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
module Audio where

import System.IO.Unsafe

--import Foreign.Storable
import Data.IORef
import Text.Printf
import qualified Data.Vector as VV
import Data.Vector.Storable as V
  (toList,convert,Storable,Vector, slice, length, generate, (!), (++), thaw, freeze, unsafeThaw, replicate)
import Data.Vector.Storable.Mutable as MV
  (IOVector, copy, slice, generate, generateM, read, write, new, clear, grow)
import Control.Monad
import Control.Monad.Reader

import Control.Concurrent
import Control.Concurrent.STM

import Codec.Audio.Vorbis.File as Vorbis 
import Data.ByteString as BS (ByteString, index, uncons, length)
import Data.Word
import Data.Int
import Data.Bits

import SDL hiding (Vector)

-- a sound is a thing that when given a output buffer fills it with audio
-- and points you to the next sound. How could it be controlled at the
-- chunk level by IO... 
-- in addition to the 
data Sound = Sound (OutBuffer -> IO ())

type OutBuffer = IOVector Float
type Chunk = Vector Float
type Buffer = IOVector Float


data Tape = Tape { tapePtr :: Int, tapeSize :: Int, tapeData :: Chunk }

-- temporarily broken
spanTape :: Int -> Tape -> (Chunk, Tape)
spanTape n (Tape ptr size chunk) =
  if ptr + n > size
    then do
      let amount1 = size - ptr
      let amount2 = (ptr + n) - size
      (V.slice ptr amount1 chunk V.++ V.replicate amount2 0.0, Tape size size chunk)
    else (V.slice ptr n chunk, Tape (ptr + n) size chunk)

isLive :: Tape -> Bool
isLive (Tape ptr size _) = ptr < size

-- 1 Hz wave i.e. 1 cycle per second = 1 cycle per 48000 samples
-- i.e. sin(t + 2pi/48000)




-- bytestrings and chunks

intToDouble :: Int -> Double
intToDouble = fromIntegral
doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

decode16LE :: Word8 -> Word8 -> Int16
decode16LE x y =
  let xx = fromIntegral x :: Word16 in
  let yy = fromIntegral y :: Word16 in
  let zz = yy `shiftL` 8 .|. xx in
  fromIntegral zz

i16ToFloat :: Int16 -> Float
i16ToFloat i = fromIntegral i / 32768.0

bsToChunk :: ByteString -> Chunk
bsToChunk bs = V.generate (BS.length bs `div` 2) g where
  g :: Int -> Float
  g i =
    let x = bs `index` (2*i) in
    let y = bs `index` (2*i+1) in
    i16ToFloat (decode16LE x y)

-- extract a chunk from a stereo byte stream
-- i.e. llrrllrrllrrll... -> L L L L L 
bsToChunkL :: ByteString -> Chunk
bsToChunkL bs = V.generate (BS.length bs `div` 4) g where
  g :: Int -> Float
  g i =
    let x = bs `index` (4*i) in
    let y = bs `index` (4*i+1) in
    i16ToFloat (decode16LE x y)


-- get up to n total samples from a vorbis file, it may return less
-- returns Nothing on EOF, assumes there is 1 channel
vorbChunkMono :: Vorbis.File -> Int -> IO (Maybe Chunk)
vorbChunkMono file n = do
  r <- Vorbis.read file (2*n) Vorbis.LittleEndian Vorbis.SixteenBit Vorbis.Signed
  case r of
    Nothing     -> return Nothing
    Just (bs,_) -> return (Just (bsToChunk bs))

vorbChunkStereo :: Vorbis.File -> Int -> IO (Maybe Chunk)
vorbChunkStereo file n = do
  r <- Vorbis.read file (4*n) Vorbis.LittleEndian Vorbis.SixteenBit Vorbis.Signed
  case r of
    Nothing     -> return Nothing
    Just (bs,_) -> return (Just (bsToChunkL bs))
  

-- assumes mono right now
getVorb :: Vorbis.File -> Int -> IOVector Float -> IO ()
getVorb file total out = go 0 total where
  go ptr n = do
    r <- vorbChunkMono file n
    case r of
      Nothing -> do
        blank <- MV.new n
        MV.copy (MV.slice ptr n out) blank
      Just chunk -> do
        let m = V.length chunk
        copyy (MV.slice ptr m out) chunk
        if m < n
          then go (ptr + m) (n - m)
          else return ()


-- a source which draws from a vorbis file
vorbTaker :: Vorbis.File -> IOVector Float -> IO ()
vorbTaker file out = getVorb file 1024 out

{-
-- load all the data into a buffer (assumes mono)
loadVorb :: Vorbis.File -> IO Chunk
loadVorb file = do
  let size0 = 4096
  buf0 <- MV.new size0
  go buf0 0 size0 where
    go :: Buffer -> Int -> Int -> IO Chunk
    go buf ptr size = do
      result <- vorbChunkMono file 4096
      case result of
        Nothing -> do
          V.freeze (MV.slice 0 ptr buf)
        Just chunk -> do
          let m = V.length chunk
          if ptr + m > size
            then do
              let size' = size * 2
              buf' <- MV.grow buf size'
              copyy (MV.slice ptr m buf') chunk
              go buf' (ptr + m) size'
            else do
              copyy (MV.slice ptr m buf) chunk
              go buf (ptr + m) size
-}

hoardVorb :: VorbStream -> IO Chunk
hoardVorb stream = do
  let size0 = 4096
  buf0 <- MV.new size0
  stash <- go buf0 0 size0
  vsClose stream
  return stash where
    go :: Buffer -> Int -> Int -> IO Chunk
    go buf ptr size = do
      result <- vsChunk stream 4096
--vorbChunkMono file 4096
      case result of
        Nothing -> do
          V.freeze (MV.slice 0 ptr buf)
        Just chunk -> do
          let m = V.length chunk
          if ptr + m > size
            then do
              let size' = size * 2
              buf' <- MV.grow buf size'
              copyy (MV.slice ptr m buf') chunk
              go buf' (ptr + m) size'
            else do
              copyy (MV.slice ptr m buf) chunk
              go buf (ptr + m) size



-- writes a chunk piece by piece into outbuf
chunkPlayer :: IORef Int -> Chunk -> IOVector Float -> IO ()
chunkPlayer ref chunk out = do
  let magic = 1024
  ptr <- readIORef ref
  let l = V.length chunk
  if ptr >= l
    then do
      MV.clear out
    else if ptr + magic > l
      then do
        let narrow = l - ptr
        MV.clear out
        --MV.copy (MV.slice 0 narrow out) (V.slice ptr narrow chunk)
        copyy (MV.slice 0 narrow out) (V.slice ptr narrow chunk)
        writeIORef ref l
      else do
        --MV.copy out (MV.slice ptr magic chunk)
        copyy out (V.slice ptr magic chunk)
        writeIORef ref (ptr + magic)
    
  




-- make thing that outputs samples to a buffer each time
-- keeps an internal generator state
makePureTone :: IO Sound
makePureTone = do
  var <- newIORef 0
  return $ Sound (something var)

something var out = do
  c <- readIORef var
  c' <- sineGen c 0 out
  writeIORef var c'

sineGen :: Int -> Int -> IOVector Float -> IO Int
sineGen c i out = if i < 1024
  then do
    let t = intToDouble c
    let phi = 2*pi*441.0*t/48000.0
    let s = 0.125 * sin phi
    MV.write out i (doubleToFloat s)
    sineGen (c + 1) (i + 1) out
  else do
    return c

copyy :: Storable a => IOVector a -> Vector a -> IO ()
copyy dst src = do
  V.unsafeThaw src >>= MV.copy dst


audioCallback :: Sound -> AudioFormat fmt -> IOVector fmt -> IO ()
audioCallback (Sound gen) sampleFormat out = case sampleFormat of
  FloatingLEAudio -> gen out
  _ -> return ()



newVorbStreamer :: FilePath -> IO Sound
newVorbStreamer path = do
  file <- Vorbis.openFile path
  -- the file is closed when the handle is GC'd
  return (Sound (vorbTaker file))

{-
newVorbPlayer :: FilePath -> IO Sound
newVorbPlayer path = do
  file <- Vorbis.openFile path
  chunk <- loadVorb file
  Vorbis.close file
  ref <- newIORef 0
  return (Sound (chunkPlayer ref chunk))
-}
  
-- spawn a thread that mixes sources of sound
-- pushes mixed chunks into a TMVar
-- returns a source source for the audio callback
-- add or remove sound sources from another thread
newMasterSink :: TVar [Sound] -> IO Sound
newMasterSink sourcesV = go where
  go = do
    tmv <- newEmptyTMVarIO :: IO (TMVar Chunk)
    forkIO $ do
      buf1 <- MV.new 1024
      buf2 <- MV.new 1024
      forever $ do
        mixWorker sourcesV tmv buf1
        mixWorker sourcesV tmv buf2
    let f :: IOVector Float -> IO ()
        f out = do
          chunk <- atomically (takeTMVar tmv)
          copyy out chunk
    return (Sound f)
  mixWorker sourcesV chute buf = do
    -- doesn't really mix, only 1 source right now
    [Sound gen] <- atomically (readTVar sourcesV)
    gen buf
    chunk <- V.freeze buf
    atomically (putTMVar chute chunk)

-- mixes many tapes together. When each tape ends, it's discarded.
multiOneShot :: TMVar [Tape] -> OutBuffer -> IO ()
multiOneShot tapesV out = do
  tapes <- atomically (takeTMVar tapesV)
  let (chunks, tapes') = unzip $ map (spanTape 1024) tapes
  let liveTapes = filter isLive tapes'
  if Prelude.length liveTapes > 0 then print ("tapes", Prelude.length liveTapes) else return ()
  atomically (putTMVar tapesV liveTapes)
  buf <- MV.generate 1024 (\i -> sum $ map (! i) chunks)
  MV.copy out buf


data VorbStream = VorbStream
  { vsClose :: IO ()
  , vsChunk :: Int -> IO (Maybe Chunk) }

-- check file into
-- return the chunk streamer appropriate for mono or stereo
-- stereo loader will pick either L or R to get mono
openVorbSmart :: FilePath -> IO VorbStream
openVorbSmart path = do
  file <- Vorbis.openFile path
  info <- Vorbis.info file  
  case Vorbis.inChannels info of
    Vorbis.Mono -> return $ VorbStream (Vorbis.close file) (vorbChunkMono file)
    Vorbis.Stereo -> return $ VorbStream (Vorbis.close file) (vorbChunkStereo file)
    _ -> error "How many channels does this vorbis file have? (not 1 or 2)"


--setupAudio :: IO AudioDevice
setupAudio = do
{-
  ds <- fmap VV.toList SDL.getAudioDrivers
  forM_ ds $ \AudioDriver
  print ds
-}
  


  tapesV <- newTMVarIO []
  let src1 = Sound (multiOneShot tapesV)
  --src1 <- newVorbStreamer "evil.ogg"
  --src1 <- newVorbPlayer "alert1.ogg"

  --src2 <- makePureTone

  --file <- Vorbis.openFile "sounds/alert1.ogg"
  stream <- openVorbSmart "sounds/c#5.ogg"
  chunk <- hoardVorb stream
  let insertTape = do
        let t = Tape 0 (V.length chunk) chunk
        atomically (takeTMVar tapesV >>= putTMVar tapesV . (t:))

  {-
  forkIO $ do
    threadDelay (3 * 1000000)
    insertTape
  forkIO $ do
    threadDelay (4 * 1000000)
    insertTape
  forkIO $ do
    threadDelay (5 * 1000000)
    insertTape
  forkIO $ do
    threadDelay (6 * 1000000)
    insertTape
  forkIO $ do
    threadDelay (7 * 1000000)
    insertTape
  -}

  

  srcV <- newTVarIO [src1]
  master <- newMasterSink srcV

  SDL.initialize [InitAudio]
  (dev,spec) <- SDL.openAudioDevice $ OpenDeviceSpec
    { openDeviceFreq = Desire 48000
    , openDeviceFormat = Desire FloatingNativeAudio
    , openDeviceChannels = Desire SDL.Mono
    , openDeviceSamples = 1024
    , openDeviceCallback = audioCallback master
    , openDeviceUsage = ForPlayback
    , openDeviceName = Nothing }

  forkIO $ do
    threadDelay 1000000
    setAudioDevicePlaybackState dev Play

  printf "frequency = %d\n" (fromIntegral $ audioSpecFreq spec :: Int)
--  case spec of
--    AudioSpec{audioSpecFormat=x} -> printf "format = %s" (show x)
--  printf "format = %s\n" (show $ audioSpecFormat spec)
  printf "channels = %s\n" (show $ audioSpecChannels spec)
  printf "silence = %d\n" (audioSpecSilence spec)
--  printf "samples = %d\n" (audioSpecSamples spec)
  printf "size = %d\n" (audioSpecSize spec)

  return (dev,insertTape)


teardownAudio :: AudioDevice -> IO ()
teardownAudio dev = do
  closeAudioDevice dev



