{-# LANGUAGE MultiParamTypeClasses #-}

module Sound.PortAudio.Buffer where

import Sound.PortAudio
import qualified Sound.PortAudio.Base as Base
import Foreign.C.Types (CULong)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)

-- The Int item is the Length, we always assume offset is 0
class Buffer a e where
    fromForeignPtr :: ForeignPtr e -> Int -> IO (a e)
    toForeignPtr   :: a e -> IO (ForeignPtr e, Int)

-- We need to associate stream info, since we need channel sizes
type BuffStreamCallback input output a b =
        Base.PaStreamCallbackTimeInfo
    ->  [StreamCallbackFlag]
    ->  CULong
    ->  a input
    ->  b output
    ->  IO StreamResult


-- In general if you have decided to use vectors underneath, you cannot freeze them, as that will
-- mess things up since they are simply pointers underneath
buffCBtoRawCB :: (StreamFormat input, StreamFormat output, Buffer a input, Buffer b output) =>
    BuffStreamCallback input output a b -> Stream input output -> StreamCallback input output    
buffCBtoRawCB func strm = \a b c d e -> do
    fpA <- newForeignPtr_ d -- We will not free, as callback system will do that for us   
    fpB <- newForeignPtr_ e -- We will not free, as callback system will do that for us
    storeInp <- fromForeignPtr fpA (fromIntegral $ numInputChannels strm * c)
    storeOut <- fromForeignPtr fpB (fromIntegral $ numOutputChannels strm * c)
    func a b c storeInp storeOut
    

-- We assume the Buffer is long enough!!!
readBufferStream :: (Buffer a input, StreamFormat input, StreamFormat output) => Stream input output -> CULong -> a input -> IO (Maybe Error)
readBufferStream a b c = do
    (c', _) <- toForeignPtr c
    readStream a b c'


writeBufferStream :: (Buffer a output, StreamFormat input, StreamFormat output) => Stream input output -> CULong -> a output -> IO (Maybe Error)
writeBufferStream a b c = do
    (c', _) <- toForeignPtr c
    writeStream a b c'