{-# LANGUAGE TypeSynonymInstances #-}

-- |PortAudio is a cross platform audio library which supports many different operatings systems.
module Sound.PortAudio
(
    -- Enumerations
    ErrorCode(..),
    HostApiTypeId(..),
    SampleFormat(..),

    -- Data structures
    HostApiInfo(..),
    HostErrorInfo(..),
    DeviceInfo(..),
    StreamParameters(..),
    StreamInfo(..),

    -- Types
    PaFloat32, paFloat32,
    PaInt32, paInt32,
--  PaInt24, paInt24,
    PaInt16, paInt16,
    PaInt8, paInt8,
    PaUInt8, paUInt8,
    PaTime,
    PaStream,
    paNullPtr,
    HostApiIndex,
    DeviceIndex,

    -- Constants
    paNoDevice,
    paUseHostApiSpecificDeviceSpecification,

    {- Functions -}
    getVersion,
    getVersionText,

    withPortAudio,
    initialize,
    terminate,

    withStream,
    withDefaultStream,
    openDefaultStream,
    openStream,
    closeStream,
    startStream,
    stopStream,
    abortStream,
    readStream,
    writeStream,

    getStreamInfo,
    getStreamTime,
    getStreamCpuLoad,
    getStreamReadAvailable,
    getStreamWriteAvailable,

    getHostApiCount,
    getDefaultHostApi,
    getHostApiInfo,
    getDeviceCount,
    getDefaultInputDevice,
    getDefaultOutputDevice,
    getDeviceInfo,
    getSampleSize,

    hostApiTypeIdToHostApiIndex,
    hostApiDeviceIndexToDeviceIndex,

    isFormatSupported,
    isStreamStopped,
    isStreamActive,

    paSleep,

    chunk,

    standardSampleRates
)
where

{- 
 - TODO: Change DeviceIndex into a newtype so that the actual integer isn't exposed.
 - Hide the constructor and unwrap it behind the scenes.
 -}

import Sound.PortAudio.Base
import Sound.PortAudio.Helpers

import Foreign
import Control.Monad.Error


standardSampleRates :: [Double]
standardSampleRates = [8000,
                       9600,
                       11025,
                       12000,
                       16000,
                       22050,
                       24000,
                       32000,
                       44100,
                       48000,
                       88200,
                       96000,
                       192000]

-- | Put the caller to sleep for at least n milliseconds.
-- The function may sleep longer than requested so don't rely on this for accurate
-- musical timing.
paSleep :: Int -> IO ()
paSleep = paSleep_ffi

{- This set of stuff allows us to convert an
 - type into an integer. This is used when
 - creating streams.-}
class PortAudioFormat a where
    toFmt :: a -> SampleFormat

instance PortAudioFormat PaFloat32 where
    toFmt _ = PaFloat32
instance PortAudioFormat PaInt32 where
    toFmt _ = PaInt32
{- We don't want this until PaInt24 is not
 - a synonym for PaInt32
 -
 - instance PortAudioFormat PaInt24 where
 -    toFmt _ = PaInt24
-}
instance PortAudioFormat PaInt16 where
    toFmt _ = PaInt16
instance PortAudioFormat PaInt8 where
    toFmt _ = PaInt8
instance PortAudioFormat PaUInt8 where
    toFmt _ = PaUInt8

paFloat32 = (undefined :: PaFloat32)
paInt32   = (undefined :: PaInt32)
paInt16   = (undefined :: PaInt16)
paInt8    = (undefined :: PaInt8)
paUInt8   = (undefined :: PaUInt8)

-- |Library initialization function. Call this before using 
-- PortAudio. The functions getVersion, getVersionText, and
-- getErrorText may be called before initialize is called.
initialize :: IO (Either String ErrorCode) -- ^ @(Right NoError)@ on success. @(Left err)@ on failure.
initialize = runErrorT initialize'

initialize' :: ErrorT String IO ErrorCode
initialize' = do code <- liftIO $ initialize_ffi
                 case (toEnum code) of
                      NoError -> return NoError
                      otherErr -> throwError $ show otherErr

-- |Library termination function. Call this after PortAudio
-- is no longer needed.
terminate :: IO (Either String ErrorCode) -- ^ @(Right NoError)@ on success. @(Left err)@ on failure.)
terminate = runErrorT terminate'

terminate' :: ErrorT String IO ErrorCode 
terminate' = do code <- liftIO $ terminate_ffi
                case (toEnum code) of
                     NoError -> return NoError
                     otherErr -> throwError $ show otherErr

-- |Perform a port audio action in the context
-- of the PortAudio library.
--
-- This initializes the library, performs the
-- supplied actions, and then terminates the
-- library. This is the reccomended way to use
-- the library.
-- withPortAudio :: IO a -> IO (Maybe a) -- ^ Returns @(Just a)@ on success or @Nothing@ on failure.
withPortAudio :: IO a -> IO (Either String a)
withPortAudio a = runErrorT $ withPortAudio' a

withPortAudio' :: IO a -> ErrorT String IO a
withPortAudio' a = do initCode <- initialize'
                      actRet <- liftIO a
                      termCode <- terminate'
                      return actRet

-- |A special DeviceIndex value indicating that no device
-- is available or should be used.
paNoDevice :: DeviceIndex
paNoDevice = (-1)

-- |A special DeviceIndex value indicating that the device(s)
-- to be used are specified in the host api specific stream
-- info structure.
paUseHostApiSpecificDeviceSpecification :: DeviceIndex
paUseHostApiSpecificDeviceSpecification = (-2)

-- |Retrieve the number of available host APIs. Even if a host API
-- is available it may have no devices available.
getHostApiCount :: IO (Either String Int)-- ^ @(Left err)@ on failure, @(Right int)@ on success.
getHostApiCount = runErrorT getHostApiCount'

getHostApiCount' :: ErrorT String IO Int
getHostApiCount' = do c <- liftIO getHostApiCount_ffi
                      if (c < 0)
                         then throwError $ show (toEnum c :: ErrorCode)
                         else return c
    
-- |Returns the index of the default host API.
getDefaultHostApi :: IO (Either String Int)
getDefaultHostApi = runErrorT getDefaultHostApi'

getDefaultHostApi' :: ErrorT String IO Int
getDefaultHostApi' = do c <- liftIO getDefaultHostApi_ffi
                        if (c < 0)
                           then throwError $ show (toEnum c :: ErrorCode)
                           else return c


-- |Gets a structure containing information about a specific host API. FINISH
getHostApiInfo :: HostApiIndex -> IO (Maybe HostApiInfo)
getHostApiInfo = getHostApiInfo_ffi

-- |Convert a static host API uniqe identifier to a runtime host API index. FINISH
hostApiTypeIdToHostApiIndex :: HostApiTypeId -> IO HostApiIndex
hostApiTypeIdToHostApiIndex = hostApiTypeIdToHostApiIndex_ffi

-- |Convert a host-API-specific device index to a standard PortAudio device index. FINISH
hostApiDeviceIndexToDeviceIndex :: HostApiIndex -> Int -> IO DeviceIndex
hostApiDeviceIndexToDeviceIndex = hostApiDeviceIndexToDeviceIndex_ffi

-- |Retrieve the number of available devices or @Nothing@ if there are none.
getDeviceCount :: IO (Either ErrorCode DeviceIndex)
getDeviceCount = do c <- getDeviceCount_ffi
                    return $ if (c < 0)
                                then (Left (toEnum c :: ErrorCode))
                                else (Right c)

-- |Retrieve the index of the default input device or @Nothing@ if there are none.
getDefaultInputDevice :: IO (Maybe DeviceIndex)
getDefaultInputDevice = do d <- getDefaultInputDevice_ffi
                           return $ case d of
                                       (-1) -> Nothing -- See paNoDevice
                                       dev  -> Just dev

-- |Retrieve the index of the default output device or @Nothing@ if there are none.
getDefaultOutputDevice :: IO (Maybe DeviceIndex)
getDefaultOutputDevice = do d <- getDefaultOutputDevice_ffi
                            return $ case d of
                                       (-1) -> Nothing -- See paNoDevice
                                       dev  -> Just dev


-- |Retrieve a DeviceInfo structure containing information about the specified device.
getDeviceInfo :: DeviceIndex -> IO (Maybe DeviceInfo)
getDeviceInfo = getDeviceInfo_ffi


-- |Determines whether it is possible to open a stream with the specified parameters.
isFormatSupported :: (Maybe StreamParameters) -- ^ Input Parameters
                  -> (Maybe StreamParameters) -- ^ Output Parameters
                  -> Double                   -- ^ Sample Rate
                  -> IO (Either String Int) -- ^ @(Right 0)@ on supported format, @(Left err)@ otherwise.
isFormatSupported s1 s2 sr = runErrorT $ isFormatSupported' s1 s2 sr

isFormatSupported' :: (Maybe StreamParameters) -> (Maybe StreamParameters) -> Double -> ErrorT String IO Int
isFormatSupported' s1 s2 sr = do s <- liftIO $ isFormatSupported_ffi s1 s2 sr
                                 if (0 == s)
                                    then return 0
                                    else throwError $ show (toEnum s :: ErrorCode)


-- |Open a stream for input, output, or both.
openStream :: Maybe StreamParameters -- ^ Input Parameters
           -> Maybe StreamParameters -- ^ Output Parameters
           -> Double                 -- ^ Sample Rate
           -> Int                    -- ^ Frames Per Buffer
           -> IO (Either String (PaStream a)) -- ^ @(Right PaStream)@ on success, @(Left err)@ on failure.
openStream isp osp sr fpb = runErrorT $ openStream' isp osp sr fpb

openStream' :: Maybe StreamParameters
            -> Maybe StreamParameters
            -> Double
            -> Int
            -> ErrorT String IO (PaStream a)
openStream' isp osp sr fpb =
     do (ec,s) <- liftIO $ openStream_ffi isp osp sr fpb 0 nullPtr nullPtr
        let errCode = toEnum ec
        case errCode of
             NoError -> return s
             err -> throwError $ show err

-- | A Simplified version of openStream which opens the default input and\/or output device(s).
openDefaultStream :: (PortAudioFormat a)
                  => Int          -- ^ Number of input channels
                  -> Int          -- ^ Number of output channels
                  -> a -- ^ Sample Format
                  -> Double       -- ^ Sample Rate
                  -> Int          -- ^ Frames Per Buffer
                  -> IO (Either String (PaStream a)) -- ^ @(Right PaStream)@ on success, @(Left err)@ on failure.
openDefaultStream nic noc sf sr fpb = runErrorT $ openDefaultStream' nic noc sf sr fpb

openDefaultStream' :: (PortAudioFormat a) => Int -> Int -> a -> Double -> Int
                   -> ErrorT String IO (PaStream a)
openDefaultStream' nic noc sf sr fpb = do
     (ec,s) <- liftIO $ openDefaultStream_ffi nic noc (toFmt sf) sr fpb nullPtr nullPtr
     let errCode = toEnum ec
     case errCode of
          NoError -> return s
          err -> throwError $ show err

-- Generic Stream -> Error wrapper
streamToError :: (PaStream a -> IO Int) -> PaStream a -> ErrorT String IO ErrorCode
streamToError a s = do r <- liftIO $ a s
                       return (toEnum r)

-- | Close a PortAudio stream. If the audio streem is active,
-- any pending buffers are discarded as if abortStream had been called.
closeStream :: PaStream a -> IO (Either String ErrorCode)
closeStream s = runErrorT $ closeStream' s

closeStream' :: PaStream a -> ErrorT String IO ErrorCode
closeStream' = streamToError closeStream_ffi

{- Skipping PaStreamFinishedCallback -}    
{- Skipping Pa_SetStreamFinishedCallback -}    

-- | Commences audio processing.
startStream :: PaStream a -> IO (Either String ErrorCode)
startStream s = runErrorT $ startStream' s

startStream' :: PaStream a -> ErrorT String IO ErrorCode
startStream' = streamToError startStream_ffi

-- | Terminates audio processing. It blocks until all pending
-- audio buffers have been played.
stopStream :: PaStream a -> IO (Either String ErrorCode)
stopStream s = runErrorT $ stopStream' s

stopStream' :: PaStream a -> ErrorT String IO ErrorCode
stopStream' = streamToError stopStream_ffi

-- | Terminates audio processing immediately without waiting for 
-- pending buffers to complete.
abortStream :: PaStream a -> IO (Either String ErrorCode)
abortStream s = runErrorT $ abortStream' s

abortStream' :: PaStream a -> ErrorT String IO ErrorCode
abortStream' = streamToError abortStream_ffi

withDefaultStream :: (PortAudioFormat a)
                  => Int
                  -> Int
                  -> a
                  -> Double
                  -> Int
                  -> (PaStream a -> (Int, Int, Double, Int) -> IO b)
                  -> IO (Either String b)
withDefaultStream nic noc sf sr fpb a = runErrorT $ withDefaultStream' nic noc sf sr fpb a

withDefaultStream' :: (PortAudioFormat a) 
                   => Int
                   -> Int
                   -> a
                   -> Double
                   -> Int
                   -> (PaStream a -> (Int, Int, Double, Int) -> IO b)
                   -> ErrorT String IO b
withDefaultStream' nic noc sf sr fpb a = do
    ds <- openDefaultStream' nic noc sf sr fpb
    start <- startStream' ds
    r  <- liftIO $ a ds (nic, noc, sr, fpb)
    stop <- stopStream' ds
    cs <- closeStream' ds
    return r

-- | Perform an action with a stream.
withStream :: (PaStream a -> IO a) -> PaStream a -> IO (Either String a)
withStream f s = runErrorT $ withStream' f s

withStream' :: (PaStream a -> IO a) -> PaStream a -> ErrorT String IO a 
withStream' a s = do start <- startStream' s
                     r <- liftIO $ a s
                     stop  <- stopStream' s
                     return r

-- | Determines whether the stream is stopped.
isStreamStopped :: PaStream a -> IO (Either String Bool) -- ^ (Right bool) on success, (Left err) on failure.
isStreamStopped s = do r <- isStreamStopped_ffi s
                       return $ case r of
                                     0 -> (Right False)
                                     1 -> (Right True)
                                     e -> fail $ show (toEnum e :: ErrorCode)

-- | Determines whether the stream is active.
isStreamActive :: PaStream a -> IO (Either String Bool) -- ^ (Right bool) on success, (Left err) on failure.
isStreamActive s = do r <- isStreamActive_ffi s
                      return $ case r of
                                    0 -> (Right False)
                                    1 -> (Right True)
                                    e -> fail $ show (toEnum e :: ErrorCode)


-- | Retrieve a StreamInfo containing information about the specified stream.
getStreamInfo :: PaStream a -> IO (Maybe StreamInfo)
getStreamInfo = getStreamInfo_ffi

-- | Determine the current time for the stream according to the sample clock used to generate the buffer timestamps.
getStreamTime :: PaStream a -> IO (Maybe PaTime)
getStreamTime s = do r <- getStreamTime_ffi s
                     return $ case r of
                                0.0 -> Nothing
                                t -> (Just t)
                  

-- | Retrieve CPU usage information (value between 0.0 and 1.0) for the specified stream.
--
-- Note: A usage level of 0.0 is potentially an error (no specific error condition is defined by PortAudio).
getStreamCpuLoad :: PaStream a -> IO Double
getStreamCpuLoad s = getStreamCpuLoad_ffi s

-- So this one sucks because 0.0 is both a valid CPU usage value *or* an error.
-- We'll sprinkle some pixie dust here and assume 0.0 means HAPPY! all the time.
                        
-- | 32 bit floating point representation.
type PaFloat32 = Float

-- | 32 bit integer representation.
type PaInt32 = Int32

-- | 24 bit integer representation (NOT SUPPORTED YET)
type PaInt24 = Int32

-- | 16 bit integer representation
type PaInt16 = Int16

-- | 8 bit integer representation
type PaInt8 = Int8

-- | 8 bit unsigned integer representation
type PaUInt8 = Word8


{- BEGIN READERS AND WRITERS -}
-- Warning: these are scary, pointer wielding functions which are not to be trifled with.


-- | Read a sample from an input stream.
readStream :: (Storable a)
           => PaStream a             -- ^ The input stream
           -> Int                    -- ^ The number of channels
           -> Int                    -- ^ The number of frames
           -> IO (Either String [[a]])
readStream s c f = runErrorT $ readStream' s c f

readStream' :: (Storable a) => PaStream a -> Int -> Int -> ErrorT String IO [[a]]
readStream' str chns frms = do
         let len = frms * chns
             withPtr act ptr = do err <- act ptr
                                  case (toEnum err) of
                                       NoError -> peekArray len ptr >>= return . Right . chunk chns
                                       err -> return (Left err)
         r <- liftIO $ allocaArray len (withPtr (\ptr -> readStream_ffi str (castPtr ptr) len))
 
         case r of
              (Left err) -> throwError (show err)
              (Right v) -> return v
-- Thanks a bunch to paczesiowa in #haskell.


writeStream :: (Storable a) => PaStream a -- ^ The output stream
            -> [[a]]                    -- ^ The samples to be played
            -> Int                      -- ^ Number of frames
            -> IO (Either String ErrorCode) -- ^ The return status of the write
writeStream s l f = runErrorT $ writeStream' s l f


writeStream' :: (Storable a) => PaStream a -> [[a]] -> Int -> ErrorT String IO ErrorCode
writeStream' str frames numFrames = do
         r <- liftIO $ (withArray . concat) frames (\ptr -> writeStream_ffi str (castPtr ptr) (length frames))
         return (toEnum r)

{- END READERS AND WRITERS -}

-- | Get the number of frames that can be read from the stream without blocking.
getStreamReadAvailable :: PaStream a -> IO (Either String Int)
getStreamReadAvailable s = do r <- getStreamReadAvailable_ffi s
                              return $ if (r >= 0) then (Right r)
                                                   else fail $ show $ (toEnum r :: ErrorCode)

-- | Get the number of frames which can be written to the stream without blocking.
getStreamWriteAvailable :: PaStream a -> IO (Either String Int)
getStreamWriteAvailable s = do r <- getStreamWriteAvailable_ffi s
                               return $ if (r >= 0) then (Right r)
                                                    else fail $ show $ (toEnum r :: ErrorCode)

-- | Retrieve the size of a given sample format in bytes.
getSampleSize :: SampleFormat -> IO (Either String Int)
getSampleSize f = do r <- getSampleSize_ffi f
                     return $ if (r < 0) then fail $ show (toEnum r :: ErrorCode)
                                         else (Right r)


