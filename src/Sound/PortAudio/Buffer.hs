{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module provides the 'Buffer' type class that abstracts the array type that is being used for I\/O. Inspired by hsndfile.

module Sound.PortAudio.Buffer where

import Sound.PortAudio
import qualified Sound.PortAudio.Base as Base
import Foreign.C.Types (CULong)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)




-- | Buffer class for I\/O on PortAudio Buffers.
class Buffer a e where
    -- | Construct a buffer from a 'ForeignPtr' and the element count.
    fromForeignPtr :: ForeignPtr e -> Int -> IO (a e)
    
    -- | Retrieve from a buffer a 'ForeignPtr' pointing to its data and an element count.
    toForeignPtr   :: a e -> IO (ForeignPtr e, Int)

-- | Type of callbacks that will be called with input samples that should provide output samples.
-- This can work with arbitrary user specified buffers, as opposed to raw C arrays (ForeignPtr).
-- See @StreamCallback@ for the more information.
type BuffStreamCallback input output a b =
        Base.PaStreamCallbackTimeInfo    -- ^ Timing information for the input and output
    ->  [StreamCallbackFlag]             -- ^ Status flags
    ->  CULong                           -- ^ # of input samples
    ->  a input                          -- ^ input samples
    ->  b output                         -- ^ where to write output samples
    ->  IO StreamResult                  -- ^ What to do with the stream, plus the output to stream



-- | Wrap a buffer callback into the generic stream callback type.
buffCBtoRawCB :: (StreamFormat input, StreamFormat output, Buffer a input, Buffer b output) =>
    BuffStreamCallback input output a b -> Stream input output -> StreamCallback input output    
buffCBtoRawCB func strm = \a b c d e -> do
    fpA <- newForeignPtr_ d -- We will not free, as callback system will do that for us   
    fpB <- newForeignPtr_ e -- We will not free, as callback system will do that for us
    storeInp <- fromForeignPtr fpA (fromIntegral $ numInputChannels strm * c)
    storeOut <- fromForeignPtr fpB (fromIntegral $ numOutputChannels strm * c)
    func a b c storeInp storeOut
    

-- | This Function is a high level version of @readStream@ which does not deal with Pointers
-- and can operate on any type which is an instance of @Buffer@, possible a storable vector. It is assumed that
-- the buffer is correctly sized, its length must be the number of frames times the channels in the underlying output stream.
readBufferStream :: (Buffer a input, StreamFormat input, StreamFormat output) => Stream input output -> CULong -> a input -> IO (Maybe Error)
readBufferStream a b c = do
    (c', _) <- toForeignPtr c
    readStream a b c'

-- | This Function is a high level version of @writeStream@ which does not deal with Pointers
-- and can operate on any type which is an instance of @Buffer@, possible a storable vector. It is assumed that
-- the buffer is correctly sized, its length must be the number of frames times the channels in the underlying output stream.
writeBufferStream :: (Buffer a output, StreamFormat input, StreamFormat output) => Stream input output -> CULong -> a output -> IO (Maybe Error)
writeBufferStream a b c = do
    (c', _) <- toForeignPtr c
    writeStream a b c'