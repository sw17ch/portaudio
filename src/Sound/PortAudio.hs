{-# LANGUAGE ScopedTypeVariables #-}
module Sound.PortAudio
    ( Error(..), errorValuesByCode, errorCodesByValue
    , initialize, terminate, withPortAudio
    , StreamCallback, StreamResult(..)
    , StreamFormat, StreamOpenFlag(..)
    , StreamParameters(..)
    , Stream, openStream, withStream, openDefaultStream, withDefaultStream, abortStream, closeStream
    , startStream, stopStream, withStreamRunning
    , addStreamFin, withDefaultOutputInfo, withDefaultInputInfo, makeFinishedCallback ) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (|||))
import Control.Exception (bracket, finally)
import Control.Monad.Instances () -- for Functor (Either a)
import Data.Bits (Bits, (.&.), (.|.))
import Data.Int (Int8, Int16, Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Foreign.C.Types (CDouble, CInt, CFloat, CULong)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, castPtr, nullFunPtr, nullPtr, freeHaskellFunPtr)
import Foreign.Storable (Storable, peek, poke)
import qualified Sound.PortAudio.Base as Base

data Error
    = NotInitialized
    | UnanticipatedHostError
    | InvalidChannelCount
    | InvalidSampleRate
    | InvalidDevice
    | InvalidFlag
    | SampleFormatNotSupported
    | BadIODeviceCombination
    | InsufficientMemory
    | BufferTooBig
    | BufferTooSmall
    | NullCallback
    | BadStreamPtr
    | TimedOut
    | InternalError
    | DeviceUnavailable
    | IncompatibleHostApiSpecificStreamInfo
    | StreamIsStopped
    | StreamIsNotStopped
    | InputOverflowed
    | OutputUnderflowed
    | HostApiNotFound
    | InvalidHostApi
    | CanNotReadFromACallbackStream
    | CanNotWriteToACallbackStream
    | CanNotReadFromAnOutputOnlyStream
    | CanNotWriteToAnInputOnlyStream
    | IncompatibleStreamHostApi
    | BadBufferPtr
    | UnknownError !CInt
      deriving (Eq, Ord, Read, Show)

errorValuesByCode :: IntMap Error
errorValuesByCode = 
    IntMap.fromList $ map (first $ fromIntegral . Base.unPaErrorCode)
              [ (Base.paNotInitialized, NotInitialized)
              , (Base.paUnanticipatedHostError, UnanticipatedHostError)
              , (Base.paInvalidChannelCount, InvalidChannelCount)
              , (Base.paInvalidSampleRate, InvalidSampleRate)
              , (Base.paInvalidDevice, InvalidDevice)
              , (Base.paInvalidFlag, InvalidFlag)
              , (Base.paSampleFormatNotSupported, SampleFormatNotSupported)
              , (Base.paBadIODeviceCombination, BadIODeviceCombination)
              , (Base.paInsufficientMemory, InsufficientMemory)
              , (Base.paBufferTooBig, BufferTooBig)
              , (Base.paBufferTooSmall, BufferTooSmall)
              , (Base.paNullCallback, NullCallback)
              , (Base.paBadStreamPtr, BadStreamPtr)
              , (Base.paTimedOut, TimedOut)
              , (Base.paInternalError, InternalError)
              , (Base.paDeviceUnavailable, DeviceUnavailable)
              , (Base.paIncompatibleHostApiSpecificStreamInfo, IncompatibleHostApiSpecificStreamInfo)
              , (Base.paStreamIsStopped, StreamIsStopped)
              , (Base.paStreamIsNotStopped, StreamIsNotStopped)
              , (Base.paInputOverflowed, InputOverflowed)
              , (Base.paOutputUnderflowed, OutputUnderflowed)
              , (Base.paHostApiNotFound, HostApiNotFound)
              , (Base.paInvalidHostApi, InvalidHostApi)
              , (Base.paCanNotReadFromACallbackStream, CanNotReadFromACallbackStream)
              , (Base.paCanNotWriteToACallbackStream, CanNotWriteToACallbackStream)
              , (Base.paCanNotReadFromAnOutputOnlyStream, CanNotReadFromAnOutputOnlyStream)
              , (Base.paCanNotWriteToAnInputOnlyStream, CanNotWriteToAnInputOnlyStream)
              , (Base.paIncompatibleStreamHostApi, IncompatibleStreamHostApi)
              , (Base.paBadBufferPtr, BadBufferPtr) ]

errorCodesByValue :: Map Error Int
errorCodesByValue = Map.fromList $ map swap $ IntMap.toList errorValuesByCode
    where swap (a,b) = (b,a)

instance Enum Error where
    toEnum i = fromMaybe (UnknownError $ fromIntegral i) $ IntMap.lookup i errorValuesByCode

    fromEnum (UnknownError i) = fromIntegral i
    fromEnum other            = fromJust $ Map.lookup other errorCodesByValue

-- | Convert a @CInt@ into a @Maybe Error@
maybeError :: CInt -> Maybe Error
maybeError i | i == Base.unPaErrorCode Base.paNoError = Nothing
             | otherwise                              = Just $ toEnum $ fromIntegral i


-- | Initialize PortAudio. All calls to @initialize@ should be matched by a call to @terminate@, or
-- use @withPortAudio@ to terminate automatically
initialize :: IO (Maybe Error)
initialize = maybeError <$> Base.pa_Initialize

-- | Terminate PortAudio.
terminate :: IO (Maybe Error)
terminate = maybeError <$> Base.pa_Terminate


-- | Evaluate a function with PortAudio initialized, terminating it after evaluation
withPortAudio
    :: IO (Either Error a) -- ^ Computation to apply while PortAudio is initialized
    -> IO (Either Error a)
withPortAudio f = go `finally` terminate
    where go = initialize >>= maybe f (return . Left)

-- | Data type of flags passed in to a stream callback to indicate various conditions of the stream
data StreamCallbackFlag
    = InputUnderflow   -- ^ Some input samples will be pure silence because there was insufficient real input data to fill the input buffer. 
    | InputOverflow    -- ^ The input data was truncated to fit in the input buffer
    | OutputUnderflow  -- ^ A gap was inserted into the real output stream due to the callback not returning quickly enough
    | OutputOverflow   -- ^ Some of the output data will be discarded because the output cannot handle it
    | PrimingOutput    -- ^ Some or all of the output data will be used to prime the stream, and the input might be zero.
      deriving (Eq, Ord, Read, Show)

-- | Turn a bitmask of callback flags as passed into a "raw" callback and give back a nice usable list of @StreamCallbackFlags@
unpackCallbackFlags :: CInt -> [StreamCallbackFlag]
unpackCallbackFlags i = [flag | (value, flag) <- flagsAndValues, i .&. value == value]
    where flagsAndValues = map (first $ fromIntegral . Base.unPaStreamCallbackFlags) 
                           [ (Base.paInputUnderflow, InputUnderflow)
                           , (Base.paInputOverflow, InputOverflow)
                           , (Base.paOutputUnderflow, OutputUnderflow)
                           , (Base.paOutputOverflow, OutputOverflow)
                           , (Base.paPrimingOutput, PrimingOutput) ]

-- | Data type of flags passed when opening a stream to set various parameters about the stream processing
data StreamOpenFlag
    = ClipOff            -- ^ Don't clip of out of range samples
    | DitherOff          -- ^ Don't dither samples
    | NeverDropInput     -- ^ Request a full duplex stream that never drops input without calling the callback.
                         -- Only valid with framesPerBuffer unspecified
    | PrimeOutputBuffers -- ^ Prime the output buffers by calling the callback first, rather than filling with zeroes
      deriving (Eq, Ord, Read, Show)

-- | Turn a list of stream opening flags into a bitmask
packOpenFlags :: [StreamOpenFlag] -> Base.PaStreamFlags
packOpenFlags = Base.PaStreamFlags . foldl' (\ bm f -> bm .|. Map.findWithDefault 0 f flagsAndValues) 0
    where flagsAndValues =
              Map.fromList $ map (second $ fromIntegral . Base.unPaStreamFlags)
                     [ (ClipOff, Base.paClipOff)
                     , (DitherOff, Base.paDitherOff)
                     , (NeverDropInput, Base.paNeverDropInput)
                     , (PrimeOutputBuffers, Base.paPrimeOutputBuffersUsingStreamCallback) ]

-- | Type of callbacks that will be called with input samples that should provide output samples.
type StreamCallback input output =
      Base.PaStreamCallbackTimeInfo -- ^ Timing information for the input and output
   -> [StreamCallbackFlag]          -- ^ Status flags
   -> CULong                        -- ^ # of input samples
   -> Ptr input                     -- ^ input samples
   -> Ptr output                    -- ^ where to write output samples
   -> IO StreamResult               -- ^ What to do with the stream, plus the output to stream


-- | Result from stream callbacks that determines the action to take regarding the stream
data StreamResult
    = Continue -- ^ Continue running the stream and calling the stream callbakc
    | Complete -- ^ Complete the stream, continuing to emit sound until the output generated by the callback is all played
    | Abort    -- ^ Abort the stream as soon as possible
      deriving (Eq, Ord, Read, Show)

streamResultToPaStreamResult :: StreamResult -> Base.PaStreamCallbackResult
streamResultToPaStreamResult Continue = Base.paContinue
streamResultToPaStreamResult Complete = Base.paComplete
streamResultToPaStreamResult Abort    = Base.paAbort



-- Class of stream formats that can be used with PortAudio
class Storable a => StreamFormat a where
    -- | Constant value to pass in underlying stream parameters. a can be undefined
    paSampleFormat :: a -> Base.PaSampleFormat

instance StreamFormat Int8   where paSampleFormat _ = Base.paInt8
instance StreamFormat Int16  where paSampleFormat _ = Base.paInt16
instance StreamFormat Int32  where paSampleFormat _ = Base.paInt32
instance StreamFormat CFloat where paSampleFormat _ = Base.paFloat32


-- | A particular configuration of parameters for a stream
data StreamParameters format 
    = StreamParameters
      { spDevice           :: !Base.PaDeviceIndex
      , spChannelCount     :: !CInt
      , spSuggestedLatency :: !Base.PaTime }        

-- | A PortAudio Stream
data Stream
    = Stream
      { underlyingStream   :: !(Ptr Base.PaStream)
      , underlyingCallback :: !Base.PaStreamCallbackFunPtr }

instance Show Stream where
    show _ = "<Stream>"

-- | Wrap a "cooked" callback into a raw one suitable for passing to the underlying library
wrapCallback
    :: (StreamFormat input, StreamFormat output) => 
       StreamCallback input output
    -> IO Base.PaStreamCallbackFunPtr
wrapCallback callback = 
    Base.wrap_PaStreamCallback $ \ input output frameCount ptrTimeInfo statusFlags _ ->
        peek ptrTimeInfo >>= \ timeInfo ->
        Base.unPaStreamCallbackResult . streamResultToPaStreamResult <$>
            callback timeInfo (unpackCallbackFlags statusFlags) frameCount (castPtr input) (castPtr output)

-- | Allocate space for convert the given "cooked" stream parameters into "raw" stream parameters, then evaluate
-- the function over it
withPtrPaStreamParameters
    :: forall format a. (StreamFormat format) =>
       Maybe (StreamParameters format)
    -> (Ptr Base.PaStreamParameters -> IO a) -> IO a
withPtrPaStreamParameters Nothing   f = f nullPtr
withPtrPaStreamParameters (Just sp) f =
    let params = Base.PaStreamParameters
                      (spDevice sp)
                      (spChannelCount sp)
                      (paSampleFormat (undefined :: format))
                      (spSuggestedLatency sp)
                      nullPtr
    in alloca $ \ ptr -> poke ptr params >> f ptr

-- | Helper function which does the allocation and cleanup lifting for opening a stream.
openStream_
    :: (StreamFormat input, StreamFormat output) =>
       Maybe (StreamCallback input output)
    -> (Ptr (Ptr Base.PaStream) -> Base.PaStreamCallbackFunPtr -> IO CInt)
    -> IO (Either Error Stream)
openStream_ maybeCallback f =
    alloca $ \ ptrPtrStream -> 
        do wrappedCallback <- maybe (return nullFunPtr) wrapCallback maybeCallback
           result <- maybeError <$> (f ptrPtrStream wrappedCallback)
           case result of
             Just err | isJust maybeCallback -> freeHaskellFunPtr wrappedCallback >> return (Left err)
             Just err                        ->                                      return (Left err)

             Nothing  -> peek ptrPtrStream >>= \ ptrStream -> return (Right $! Stream ptrStream wrappedCallback)
                                        
-- | Open a particular PortAudio stream using custom stream parameters. input and output are the type of sample
-- format requested
openStream
    :: (StreamFormat input, StreamFormat output) =>
       Maybe (StreamParameters input)      -- ^ Input parameters
    -> Maybe (StreamParameters output)     -- ^ Output parameters
    -> CDouble                             -- ^ Sample rate
    -> Maybe CULong                        -- ^ Frames per buffer
    -> [StreamOpenFlag]                    -- ^ Stream flags
    -> Maybe (StreamCallback input output) -- ^ Callback, or @Nothing@ for a blocking read/write stream
    -> IO (Either Error Stream)
openStream maybeInputParams maybeOutputParams sampleRate framesPerBuffer flags callback =
    openStream_ callback $ \ ptrPtrStream wrappedCallback ->
    withPtrPaStreamParameters maybeInputParams $ \ inputParams ->
    withPtrPaStreamParameters maybeOutputParams $ \ outputParams ->
        Base.pa_OpenStream 
            ptrPtrStream
            inputParams
            outputParams
            sampleRate
            (maybe Base.paFramesPerBufferUnspecified fromIntegral framesPerBuffer)
            (packOpenFlags flags)
            wrappedCallback
            nullPtr

-- | Open a stream as in @openStream@ then apply some computation to the opened stream, closing it automatically
-- afterwards
withStream
    :: (StreamFormat input, StreamFormat output) =>
       Maybe (StreamParameters input)      -- ^ Input parameters
    -> Maybe (StreamParameters output)     -- ^ Output parameters
    -> CDouble                             -- ^ Sample rate
    -> Maybe CULong                        -- ^ Frames per buffer
    -> [StreamOpenFlag]                    -- ^ Stream flags
    -> Maybe (StreamCallback input output) -- ^ Callback, or @Nothing@ for a blocking read/write stream
    -> (Stream -> IO (Either Error a))     -- ^ Computation to apply
    -> IO (Either Error a)
withStream maybeInputParams maybeOutputParams sampleRate framesPerBuffer flags callback =
    bracket open close . (return . Left |||)
        where open = openStream maybeInputParams maybeOutputParams sampleRate framesPerBuffer flags callback
              close = const (return Nothing) ||| closeStream

-- | Open the default PortAudio stream
openDefaultStream
    :: forall format. (StreamFormat format) =>
       Int                                  -- ^ Number of input channels
    -> Int                                  -- ^ Number of output channels
    -> CDouble                              -- ^ Sample rate
    -> Maybe CULong                         -- ^ Frames per buffer
    -> Maybe (StreamCallback format format) -- ^ Callback, or @Nothing@ for a blocking read/write stream
    -> IO (Either Error Stream)
openDefaultStream numInputs numOutputs sampleRate framesPerBuffer callback =
    openStream_ callback $ \ ptrPtrStream wrappedCallback -> 
        Base.pa_OpenDefaultStream 
            ptrPtrStream
            (fromIntegral numInputs)
            (fromIntegral numOutputs)
            (paSampleFormat (undefined :: format))
            sampleRate
            (fromMaybe Base.paFramesPerBufferUnspecified framesPerBuffer)
            wrappedCallback
            nullPtr


-- | Open a stream as in @openDefaultStream@ then apply some computation to the opened stream, closing it
-- automatically afterwards
withDefaultStream
    :: (StreamFormat format) =>
       Int                                  -- ^ Number of input channels
    -> Int                                  -- ^ Number of output channels
    -> CDouble                              -- ^ Sample rate
    -> Maybe CULong                         -- ^ Frames per buffer
    -> Maybe (StreamCallback format format) -- ^ Callback, or @Nothing@ for a blocking read/write stream
    -> (Stream -> IO (Either Error a))      -- ^ Computation to apply
    -> IO (Either Error a)
withDefaultStream numInputs numOutputs sampleRate framesPerBuffer callback =
    bracket open close . (return . Left |||)
        where open = openDefaultStream numInputs numOutputs sampleRate framesPerBuffer callback
              close = const (return Nothing) ||| closeStream



-- Not sure if these can be replaced in place of the above with Methods.
-- I think they could still be useful for finer grained control. I'll try to 
-- write examples where we can't use the above methods in order to demonstrate.

addStreamFin :: Base.PaStreamFinishedCallback -> Stream -> IO (Maybe Error)
addStreamFin callback strm = do
    wrapped <- Base.wrap_PaStreamFinishedCallback callback
    maybeError <$> Base.pa_SetStreamFinishedCallback (underlyingStream strm) wrapped

getDefaultOut :: IO (Either Error Base.PaDeviceIndex)
getDefaultOut = do
    out <- Base.PaDeviceIndex <$> Base.pa_GetDefaultOutputDevice
    return $ if out /= Base.paNoDevice then (Right out) else (Left DeviceUnavailable)

getDefaultIn :: IO (Either Error Base.PaDeviceIndex)
getDefaultIn = do
    in_ <- Base.PaDeviceIndex <$> Base.pa_GetDefaultInputDevice
    return $ if in_ /= Base.paNoDevice then (Right in_) else (Left DeviceUnavailable)

--prolly can simplify with the either monad
withDefaultOutputInfo :: ((Base.PaDeviceIndex, Base.PaDeviceInfo) -> IO (Either Error a)) -> IO (Either Error a)
withDefaultOutputInfo func = do
    out <- getDefaultOut
    case out of
        Left err -> return $ Left err
        Right val -> do
            outInfo <- getDeviceInfo val
            case outInfo of
                Left err2 -> return $ Left err2
                Right info -> func (val,info)

withDefaultInputInfo :: ((Base.PaDeviceIndex, Base.PaDeviceInfo) -> IO (Either Error a)) -> IO (Either Error a)
withDefaultInputInfo func = do
    _in <- getDefaultIn
    case _in of
        Left err -> return $ Left err
        Right val -> do
            outInfo <- getDeviceInfo val
            case outInfo of
                Left err2 -> return $ Left err2
                Right info -> func (val,info)

getDeviceInfo :: Base.PaDeviceIndex -> IO (Either Error Base.PaDeviceInfo)
getDeviceInfo x = do
    devInfo <- Base.pa_GetDeviceInfo (Base.unPaDeviceIndex x)
    if devInfo == nullPtr
        then return (Left DeviceUnavailable)
        else do
            devStruct <- peek devInfo
--           free devInfo
            return (Right devStruct)

-- If we don't care about user data, just use this
makeFinishedCallback :: IO () -> Base.PaStreamFinishedCallback
makeFinishedCallback a = \_ -> a


-- | Abort audio processing of the stream, stopping output as soon as possible.
-- Output buffers that haven't already been committed.
abortStream  :: Stream -> IO (Maybe Error)
abortStream = fmap maybeError . Base.pa_AbortStream . underlyingStream

-- | Close a stream, releasing the resources held for it.
closeStream :: Stream -> IO (Maybe Error)
closeStream s =
    do freeHaskellFunPtr $ underlyingCallback s
       maybeError <$> Base.pa_CloseStream (underlyingStream s)

-- | Start audio processing on the stream.
-- The callback will begin being called, if this is a callback style stream.
startStream :: Stream -> IO (Maybe Error)
startStream = fmap maybeError . Base.pa_StartStream . underlyingStream

-- | Stop audio processing for the stream.
-- Output buffers already provided will be completed before audio processing halts.
stopStream :: Stream -> IO (Maybe Error)
stopStream = fmap maybeError . Base.pa_StopStream . underlyingStream

-- | Bracket a computation with starting and stopping the stream
withStreamRunning :: Stream -> (Stream -> IO (Either Error a)) -> IO (Either Error a)
withStreamRunning s = bracket start stop . (return . Left |||)
    where start = maybe (Right s) Left <$> startStream s
          stop = const (return Nothing) ||| stopStream
