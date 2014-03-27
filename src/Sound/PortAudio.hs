{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

-- | This is the primary entry point to the PortAudio Haskell Interface (http://www.portaudio.com/).
-- This interface supports both blocking and callback based IO.
module Sound.PortAudio
    (
    -- *Port Audio Initialization
      initialize, terminate, withPortAudio

    -- *Stream Setup
    -- | These methods can only be called once you have initialized PortAudio.
    --  In most cases when starting a stream you should use @withDefaultStream@.
    --  See the example code for simple use cases.
    
    -- | The type parameters on Stream must correspond to those in @StreamFormat@.
    , Stream, StreamParameters(..)
    , StreamFormat
    , openStream, openDefaultStream, closeStream, abortStream
    , withStream, withDefaultStream
    , FinCallback
    
    -- **Reading and Writing from a Stream
    , readAvailable, writeAvailable
    , startStream, stopStream, readStream, writeStream

    -- **Stream Utilities
    , numInputChannels, numOutputChannels
    , getNumDevices, getStreamInfo, getDeviceInfo, getDefaultInputInfo
    , getDefaultOutputInfo
    , getStreamTime 

    -- *Port Audio Callback IO
    -- **Types
    , StreamCallback, StreamResult(..), StreamCallbackFlag(..)
    -- *Error Codes and Flags
    , StreamOpenFlag(..)
    , Error(..)) where

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
import Data.Maybe (fromMaybe, fromJust, isJust)
import qualified Data.Map as Map
import Foreign.C.Types (CInt, CFloat, CULong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullFunPtr, nullPtr, freeHaskellFunPtr)
import Foreign.Storable (Storable, peek, poke)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)

import qualified Sound.PortAudio.Base as Base

-- | These are the various Error Codes that can be returned by PortAudio
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

-- | Safely frees a @FunPtr@
safeFreeHaskellFunPtr :: FunPtr a -> IO ()
safeFreeHaskellFunPtr ptr | ptr == nullFunPtr = return ()
                          | otherwise         = freeHaskellFunPtr ptr

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



-- | If you want to use the callback IO method in PortAudio, your callback function should have the following
-- type signature. These are callbacks that will be called with input samples and should provide output samples.
type StreamCallback input output =
      Base.PaStreamCallbackTimeInfo -- ^ Timing information for the input and output
   -> [StreamCallbackFlag]          -- ^ Status flags
   -> CULong                        -- ^ # of input samples
   -> Ptr input                     -- ^ input samples
   -> Ptr output                    -- ^ where to write output samples
   -> IO StreamResult               -- ^ What to do with the stream, plus the output to stream


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






-- | Result from stream callbacks that determines the action to take regarding the stream
data StreamResult
    = Continue -- ^ Continue running the stream and calling the stream callback
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





-- | Initialize PortAudio. All calls to @initialize@ should be matched by a call to @terminate@, or
-- use @withPortAudio@ to terminate automatically

-- | A particular configuration of parameters for a stream. Various Device Info for setting up these parameters
-- can be accessed through @getNumDevices@, @getDeviceInfo@, @getDefaultInputInfo, @getDefaultOutputInfo@. 
data StreamParameters format 
    = StreamParameters
      { spDevice           :: !Base.PaDeviceIndex -- ^ The Device Number identified by PortAudio for the Device of Interest
      , spChannelCount     :: !CInt               -- ^ The Number of Channels to Register with the Device
      , spSuggestedLatency :: !Base.PaTime        -- ^ The average Time with Which to Offset the Playing of Audio,
                                                  -- in case you need to do some Time Consuming Computation before writing to the stream.
      }


-- | A PortAudio Stream
data Stream a b
    = Stream
      { underlyingStream      :: !(Ptr Base.PaStream)          -- ^ Pointer to be Passed to PortAudio, representing the Underlying Stream
      , underlyingCallback    :: !Base.PaStreamCallbackFunPtr  -- ^ Pointer used by PortAudio to callback into Haskell for Buffer Processing.
                                                               -- If this is a nullPtr, then we are using BlockingIO, otherwise we use CallbackIO.
      , underlyingFinCallback :: !Base.PaStreamFinishedCallbackFunPtr -- ^ Pointer to IO function called when stream has been flushed and terminated.
      , numInputChannels      :: CULong         -- ^ Number of Input Channels
      , numOutputChannels     :: CULong         -- ^ Number of Output Channels
      }

instance Show (Stream a b) where
    show _ = "<Stream>"

-- | Wrap a "cooked" callback into a raw one suitable for passing to the underlying library
wrapCallback :: (StreamFormat input, StreamFormat output) => StreamCallback input output -> IO Base.PaStreamCallbackFunPtr
wrapCallback callback = 
    Base.wrap_PaStreamCallback $ \ input output frameCount ptrTimeInfo statusFlags _ ->
        peek ptrTimeInfo >>= \ timeInfo ->
        Base.unPaStreamCallbackResult . streamResultToPaStreamResult <$>
            callback timeInfo (unpackCallbackFlags statusFlags) frameCount (castPtr input) (castPtr output)

-- | IO function to be executed once PortAudio has Flushed Buffers and Terminated
type FinCallback = IO ()

-- | Wrap a "cooked" finish callback into a raw one suitable for passing to the underlying library
wrapFinCallback :: FinCallback -> IO Base.PaStreamFinishedCallbackFunPtr
wrapFinCallback a = Base.wrap_PaStreamFinishedCallback (\_ -> a)



-- | Allocate space for convert the given "cooked" stream parameters into "raw" stream parameters, then evaluate
-- the function over it
withPtrPaStreamParameters :: forall format a. (StreamFormat format) => Maybe (StreamParameters format) -> (Ptr Base.PaStreamParameters -> IO a) -> IO a
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
openStream_ :: (StreamFormat input, StreamFormat output) =>
               Maybe (StreamCallback input output)
            -> (Ptr (Ptr Base.PaStream) -> Base.PaStreamCallbackFunPtr -> IO CInt)
            -> (Ptr Base.PaStream -> Base.PaStreamCallbackFunPtr -> IO (Either Error (Stream input output)))
            -> IO (Either Error (Stream input output))
openStream_ maybeCallback f1 f2 = alloca $ \ ptrPtrStream -> 
    do  wrappedCallback <- maybe (return nullFunPtr) wrapCallback maybeCallback
        result <- maybeError <$> (f1 ptrPtrStream wrappedCallback)
        case result of
            Just err | isJust maybeCallback -> freeHaskellFunPtr wrappedCallback >> return (Left err)
            Just err                        ->                                      return (Left err)
            Nothing  -> peek ptrPtrStream >>= \ ptrStream -> f2 ptrStream wrappedCallback




-- | Open a PortAudio stream with custom Input and Output Device Parameters. This should be closed with @closeStream@
-- or you may simply use withStream, which will close the stream for you.
openStream :: (StreamFormat input, StreamFormat output) =>
                      Maybe (StreamParameters input)        -- ^ Stream Parameters for Input Device, or @Nothing@ if we don't need Audio input for this Stream
                   -> Maybe (StreamParameters output)       -- ^ Stream Parameters for Output Device, or @Nothing@ if we don't need Audio output for this Stream
                   -> Double                                -- ^ Sample Rate for Input and Output Devices, if you need distinct Sample Rates for input and output,
                                                            --   then you should create seperate Streams
                   -> Maybe Int                             -- ^ Requested Frames Per Buffer, or @Nothing@ if you'd prefer the underlying library to send you the amount it sees fit
                   -> [StreamOpenFlag]                      -- ^ Various Parameters Dictating the Operation of the Callback Function
                   -> Maybe (StreamCallback input output)   -- ^ Callback, or @Nothing@ for a blocking read/write stream
                   -> Maybe FinCallback                     -- ^ Callback on Completion, or @Nothing@ if no final processing necessary
                   -> IO (Either Error (Stream input output))
openStream maybeInputParams maybeOutputParams sampleRate framesPerBuffer flags callback fin = let
    func1 :: Ptr (Ptr Base.PaStream) -> Base.PaStreamCallbackFunPtr -> IO CInt
    func1 ptrPtrStream wrappedCallback = do
        withPtrPaStreamParameters maybeInputParams $ \inputParams ->
            withPtrPaStreamParameters maybeOutputParams $ \outputParams ->
                Base.pa_OpenStream
                    ptrPtrStream
                    inputParams
                    outputParams
                    (realToFrac sampleRate)
                    (maybe Base.paFramesPerBufferUnspecified fromIntegral framesPerBuffer)
                    (packOpenFlags flags)
                    wrappedCallback
                    nullPtr
    
    numInp = fromMaybe 0 (fromIntegral . spChannelCount <$> maybeInputParams)
    numOut = fromMaybe 0 (fromIntegral . spChannelCount <$> maybeOutputParams)
    
    func2 :: Ptr Base.PaStream -> Base.PaStreamCallbackFunPtr -> IO (Either Error (Stream input output))
    func2 ptrStream cb = case fin of
        Nothing -> return . Right $ Stream ptrStream cb nullFunPtr numInp numOut 
        Just func -> do
            finWrapped <- wrapFinCallback func
            result <- maybeError <$> Base.pa_SetStreamFinishedCallback ptrStream finWrapped
            case result of
                Just err -> freeHaskellFunPtr finWrapped >> return (Left err)
                Nothing  -> return . Right $ Stream ptrStream cb finWrapped numInp numOut
    
    in openStream_ callback func1 func2
    



-- | Open the default PortAudio stream. This should be closed with @closeStream@
-- or you may simply use @withDefaultStream@, which will close the stream for you.
openDefaultStream :: forall format. (StreamFormat format) =>
                     Int                                  -- ^ Number of input channels
                  -> Int                                  -- ^ Number of output channels
                  -> Double                               -- ^ Sample rate
                  -> Maybe Int                            -- ^ Frames per buffer
                  -> Maybe (StreamCallback format format) -- ^ Callback, or @Nothing@ for a blocking read/write stream
                  -> Maybe FinCallback                    -- ^ Callback on Completion, or @Nothing@ if no final processing necessary
                  -> IO (Either Error (Stream format format))
openDefaultStream numInputs numOutputs sampleRate framesPerBuffer callback fin = let
    func1 :: Ptr (Ptr Base.PaStream) -> Base.PaStreamCallbackFunPtr -> IO CInt
    func1 ptrPtrStream wrappedCallback = do
        Base.pa_OpenDefaultStream 
            ptrPtrStream
            (fromIntegral numInputs)
            (fromIntegral numOutputs)
            (paSampleFormat (undefined :: format))
            (realToFrac sampleRate)
            (fromMaybe Base.paFramesPerBufferUnspecified (fromIntegral <$> framesPerBuffer))
            wrappedCallback
            nullPtr
    
    inps = fromIntegral numInputs
    outs = fromIntegral numOutputs
    
    func2 :: Ptr Base.PaStream -> Base.PaStreamCallbackFunPtr -> IO (Either Error (Stream format format))
    func2 ptrStream cb = case fin of
        Nothing -> return . Right $ Stream ptrStream cb nullFunPtr inps outs
        Just func -> do
            finWrapped <- wrapFinCallback func
            result <- maybeError <$> Base.pa_SetStreamFinishedCallback ptrStream finWrapped
            case result of
                Just err -> freeHaskellFunPtr finWrapped >> return (Left err)
                Nothing  -> return . Right $ Stream ptrStream cb finWrapped inps outs
                    
    in openStream_ callback func1 func2





-- | Open a stream as in @openStream@ then apply some computation to the opened stream, closing it automatically
-- afterwards
withStream :: (StreamFormat input, StreamFormat output) =>
       Maybe (StreamParameters input)                   -- ^ Input parameters
    -> Maybe (StreamParameters output)                  -- ^ Output parameters
    -> Double                                           -- ^ Sample rate
    -> Maybe Int                                        -- ^ Frames per buffer
    -> [StreamOpenFlag]                                 -- ^ Stream flags
    -> Maybe (StreamCallback input output)              -- ^ Callback, or @Nothing@ for a blocking read/write stream
    -> Maybe FinCallback                                -- ^ Finished Callback, or @Nothing@ for no final callback
    -> (Stream input output -> IO (Either Error a))     -- ^ Computation to apply
    -> IO (Either Error a)
withStream maybeInputParams maybeOutputParams sampleRate framesPerBuffer flags callback fin =
    bracket open close . (return . Left |||)
        where open = openStream maybeInputParams maybeOutputParams sampleRate framesPerBuffer flags callback fin
              close = const (return Nothing) ||| closeStream


-- | Open a stream as in @openDefaultStream@ then apply some computation to the opened stream, closing it
-- automatically afterwards
withDefaultStream
    :: (StreamFormat format) =>
        Int                                                -- ^ Number of input channels
     -> Int                                                -- ^ Number of output channels
     -> Double                                             -- ^ Sample rate
     -> Maybe Int                                          -- ^ Frames per buffer
     -> Maybe (StreamCallback format format)               -- ^ Callback, or @Nothing@ for a blocking read/write stream
     -> Maybe FinCallback                                  -- ^ Finished Callback, or @Nothing@ for no final callba
     -> (Stream format format -> IO (Either Error a))      -- ^ Computation to apply
     -> IO (Either Error a)
withDefaultStream numInputs numOutputs sampleRate framesPerBuffer callback fin =
    bracket open close . (return . Left |||)
        where open = openDefaultStream numInputs numOutputs sampleRate framesPerBuffer callback fin
              close = const (return Nothing) ||| closeStream



-- | Retrieve the Default Output Device Number, or an @Error@ possibly because PortAudio was not initialized.
getDefaultOut :: IO (Either Error Base.PaDeviceIndex)
getDefaultOut = do
    out <- Base.PaDeviceIndex <$> Base.pa_GetDefaultOutputDevice
    return $ if out /= Base.paNoDevice then (Right out) else (Left DeviceUnavailable)


-- | Retrieve the Default Input Device Number, or an @Error@ possibly because PortAudio was not initialized.
getDefaultIn :: IO (Either Error Base.PaDeviceIndex)
getDefaultIn = do
    in_ <- Base.PaDeviceIndex <$> Base.pa_GetDefaultInputDevice
    return $ if in_ /= Base.paNoDevice then (Right in_) else (Left DeviceUnavailable)


-- | Retrieve the Default Output Device Number and Information, or an @Error@ possibly because PortAudio was not initialized.
getDefaultOutputInfo :: IO (Either Error (Base.PaDeviceIndex, Base.PaDeviceInfo))
getDefaultOutputInfo = do
    out <- getDefaultOut
    case out of
        Left err -> return $ Left err
        Right val -> do
            outInfo <- getDeviceInfo val
            case outInfo of
                Left err2 -> return $ Left err2
                Right info -> return $ Right (val,info)


-- | Retrieve the Default Input Device Number and Information, or an @Error@ possibly because PortAudio was not initialized.
getDefaultInputInfo :: IO (Either Error (Base.PaDeviceIndex, Base.PaDeviceInfo))
getDefaultInputInfo = do
    _in <- getDefaultIn
    case _in of
        Left err -> return $ Left err
        Right val -> do
            outInfo <- getDeviceInfo val
            case outInfo of
                Left err2 -> return $ Left err2
                Right info -> return $ Right (val, info)

-- | Given a PortAudio Device Number, retrieve the corresponding Information for it. This may return an @Error@ possibly due
-- to the fact that the Device Number was invalid (devices must fall in range 0 -> getNumDevices).
getDeviceInfo :: Base.PaDeviceIndex -> IO (Either Error Base.PaDeviceInfo)
getDeviceInfo x = do
    devInfo <- Base.pa_GetDeviceInfo (Base.unPaDeviceIndex x)
    if devInfo == nullPtr
        then return (Left DeviceUnavailable)
        else do
            devStruct <- peek devInfo
            return (Right devStruct)

-- | Given a Stream, retrieve the information about it such as Average Latency. This may return an @Error@ possibly due
-- to the fact that the Stream has been closed.
getStreamInfo :: Stream input output -> IO (Either Error Base.PaStreamInfo)
getStreamInfo strm = do
    info <- Base.pa_GetStreamInfo (underlyingStream strm) 
    if info == nullPtr
        then return (Left BadStreamPtr)
        else do
            result <- peek info
            return (Right result) 

-- | Given a Stream, this gives the current time in seconds relative to the creation time of the stream.
-- The time values are monotonically increasing and have unspecified origin. This can be useful for Syncronization, such as with Midi.
getStreamTime :: Stream input output -> IO (Either Error Base.PaTime)
getStreamTime strm = do
    val <- Base.pa_GetStreamTime (underlyingStream strm)
    if (Base.unPaTime val == 0)
        then return (Left BadStreamPtr)
        else return (Right val)

-- | Get the number of devices on your machine, which can be inspected with @getDeviceInfo@.
getNumDevices :: IO Int
getNumDevices = fromIntegral <$> Base.pa_GetDeviceCount



-- | Abort audio processing of the stream, stopping output as soon as possible.
-- Output buffers that haven't already been committed.
abortStream  :: Stream a b -> IO (Maybe Error)
abortStream = fmap maybeError . Base.pa_AbortStream . underlyingStream

-- | Close a stream, releasing the resources held for it.
closeStream :: Stream a b -> IO (Maybe Error)
closeStream s =
    do closeResult <- Base.pa_CloseStream (underlyingStream s)
       safeFreeHaskellFunPtr $ underlyingCallback s
       safeFreeHaskellFunPtr $ underlyingFinCallback s
       return $ maybeError closeResult

-- | Start audio processing on the stream.
-- The callback will begin being called, if this is a callback style stream.
startStream :: Stream input output -> IO (Maybe Error)
startStream = fmap maybeError . Base.pa_StartStream . underlyingStream

-- | Stop audio processing for the stream.
-- Output buffers already provided will be completed before audio processing halts.
stopStream :: Stream input output -> IO (Maybe Error)
stopStream = fmap maybeError . Base.pa_StopStream . underlyingStream



-- | Raw Interface for Reading Data In from a Stream. This function will block until the total number
-- of requested frames are read into the array. If you want to return immediately, see @readAvailable@
-- to get the number of Frames to specify. It is assumed that the array is correctly sized, its length must be the
-- number of frames times the channels in the underlying input stream. 
readStream :: Stream input output -> CULong -> ForeignPtr input -> IO (Maybe Error)
readStream s frames pt = withForeignPtr pt $ \ptr -> do
    maybeError <$> Base.pa_ReadStream (underlyingStream s) (castPtr ptr) frames


-- | Raw Interface for Writing Data Out to a Stream. This function will block until the total number
-- of requested frames are written into the portAudio Buffers. If you want to return immediately, see @writeAvailable@
-- to get the number of Frames to specify. It is assumed that the array is correctly sized, its length must be the
-- number of frames times the channels in the underlying output stream.
writeStream :: Stream input output -> CULong -> ForeignPtr output -> IO (Maybe Error)
writeStream s frames pt = withForeignPtr pt $ \ptr -> do
    maybeError <$> Base.pa_WriteStream (underlyingStream s) (castPtr ptr) frames



-- | Given a stream, returns the number of Frames which can be consumed read immediately from the input Device.
readAvailable :: Stream input output -> IO (Either Error Int)
readAvailable strm = do
    val <- Base.pa_GetStreamReadAvailable (underlyingStream strm) 
    if (val < 0)
        then return $ Left . fromJust $ maybeError (fromIntegral val)
        else return $ Right (fromIntegral val)

-- | Given a stream, returns the number of Frames which can be written immediately to the output device.
writeAvailable :: Stream input output -> IO (Either Error Int)
writeAvailable strm = do
    val <- Base.pa_GetStreamWriteAvailable (underlyingStream strm)
    if (val < 0)
        then return $ Left . fromJust $ maybeError (fromIntegral val)
        else return $ Right (fromIntegral val)
