{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.PortAudio.Base
(
    -- FFI Functions
    abortStream_ffi,
    closeStream_ffi,
    getDefaultHostApi_ffi,
    getDefaultInputDevice_ffi,
    getDefaultOutputDevice_ffi,
    getDeviceCount_ffi,
    getDeviceInfo_ffi,
    getHostApiCount_ffi,
    getHostApiInfo_ffi,
    getLastHostErrorInfo_ffi,
    getSampleSize_ffi,
    getStreamCpuLoad_ffi,
    getStreamInfo_ffi,
    getStreamReadAvailable_ffi,
    getStreamTime_ffi,
    getStreamWriteAvailable_ffi,
    hostApiDeviceIndexToDeviceIndex_ffi,
    hostApiTypeIdToHostApiIndex_ffi,
    initialize_ffi,
    isFormatSupported_ffi,
    isStreamActive_ffi,
    isStreamStopped_ffi,
    openDefaultStream_ffi,
    openStream_ffi,
    paSleep_ffi,
    readStream_ffi,
    startStream_ffi,
    stopStream_ffi,
    terminate_ffi,
    writeStream_ffi,

    -- Types
    DeviceIndex,
    HostApiIndex,
    PaTime,

    -- Datas
    PaStream,
    HostApiInfo(..),
    SampleFormat(..),
    StreamInfo(..),
    StreamParameters(..),
    DeviceInfo(..),
    HostErrorInfo(..),

    -- Enumerations
    HostApiTypeId(..),
    ErrorCode(..),

    -- Functions
    getErrorText,
    getVersion,
    getVersionText,

    -- Other
    paNullPtr,
)
where

import C2HS
import Control.Monad.Error
import Sound.PortAudio.Helpers

#include "portaudio.h"

{# context prefix="pa" #}

paNullPtr = nullPtr

-- I don't want to expose this, rather, convert it to an ErrorCode before returning.
type PaError = Int

-- |The index of a PortAudio device.
type DeviceIndex = Int

-- |Used to enumerate host APIs at runtime. The values of
-- this range from @0@ to @(getHostApiCount - 1)@
type HostApiIndex = Int

-- |Type to represent the monotonic time in seconds which can be
-- used for synchronisation.
type PaTime = Double

-- Internal representation of a Stream Buffer. This is not to be used outside of the _ffi functions.
type PaStreamBuffer = Ptr ()

-- |A PaStream can provide multiple channels of real-time
-- streaming audio input and output to a client application.
-- type PaStream = IntPtr
data PaStream a = PaStream (IntPtr)
    deriving (Show)

-- |A structure containing information about a particular host API.
data HostApiInfo = HostApiInfo {
    hostApiInfoStructVersion :: Int,
    hostApiInfoApiType :: HostApiTypeId,
    hostApiInfoName :: String,
    hostApiInfoDeviceCount :: Int,
    hostApiInfoDefaultInputDevice :: DeviceIndex,
    hostApiInfoDefaultOutputDevice :: DeviceIndex
} deriving (Show)                                     

{- Created this enum myself to represent the
 - different sample formats which are #defined
 - in the header. I don't like #define's -}

-- |A type used to specify one or more sample formats.
--   
--   * The floating point representation (@PaFloat32@) uses +1.0 and -1.0 as the maximum and minimum.
--
--   * (@PaUInt8@) considers 128 "ground".
--
--   * The (@PaNonInterleaved@) flag indicates that a multichannel buffer is passed as a set of non-interleaved pointers. (WHAT?)
--   I don't think i want to support this.
--
data SampleFormat = PaFloat32
                  | PaInt32
                  | PaInt24
                  | PaInt16
                  | PaInt8
                  | PaUInt8
                  | PaCustomFormat
                  | PaNonInterleaved
    deriving (Read,Show)

instance Enum SampleFormat where
    fromEnum PaFloat32        = 0x00000001
    fromEnum PaInt32          = 0x00000002
    fromEnum PaInt24          = 0x00000004
    fromEnum PaInt16          = 0x00000008
    fromEnum PaInt8           = 0x00000010
    fromEnum PaUInt8          = 0x00000020
    fromEnum PaCustomFormat   = 0x00010000
    fromEnum PaNonInterleaved = 0x80000000
    toEnum 0x00000001 = PaFloat32
    toEnum 0x00000002 = PaInt32
    toEnum 0x00000004 = PaInt24
    toEnum 0x00000008 = PaInt16
    toEnum 0x00000010 = PaInt8
    toEnum 0x00000020 = PaUInt8
    toEnum 0x00010000 = PaCustomFormat
    toEnum 0x80000000 = PaNonInterleaved
    toEnum unmatched  = error ("SampleFormat.toEnum: Cannot match " ++ show  unmatched)

-- | A structure containing unchanging information about an open stream.
data StreamInfo = StreamInfo {
    streamInfoStructVersion :: Int,
    streamInfoInputLatency :: PaTime,
    streamInfoOutputLatency :: PaTime,
    streamInfoSampleRate :: Double
}
    deriving (Show)

-- |Parameters for one direction (input or output) of a stream.
data StreamParameters = StreamParameters {
    streamParametersDevice :: DeviceIndex,
    streamParametersChannelCount :: Int,
    streamParametersSampleFormat :: SampleFormat,
    streamParametersSuggestedLatency :: PaTime,
    streamParametersHostApiSpecificStreamInfo :: Ptr ()  -- This isn't to be used.
}
    deriving (Show)

instance Storable StreamParameters where
    sizeOf _    = {# sizeof StreamParameters #}
    alignment _ = alignment (undefined :: CLong)
    peek ptr    = do
        de <- liftM cIntConv   $ {# get StreamParameters.device #} ptr
        cc <- liftM cIntConv   $ {# get StreamParameters.channelCount #} ptr
        sf <- liftM cToEnum    $ {# get StreamParameters.sampleFormat #} ptr
        sl <- liftM cFloatConv $ {# get StreamParameters.suggestedLatency #} ptr
        ha <-                    {# get StreamParameters.hostApiSpecificStreamInfo #} ptr

        return $ StreamParameters {
            streamParametersDevice = de,
            streamParametersChannelCount = cc,
            streamParametersSampleFormat = sf,
            streamParametersSuggestedLatency = sl,
            streamParametersHostApiSpecificStreamInfo = ha
        }
    poke ptr (StreamParameters de cc sf sl ha) = do
        {# set StreamParameters.device #} ptr (cIntConv de)
        {# set StreamParameters.channelCount #} ptr (cIntConv cc)
        {# set StreamParameters.sampleFormat #} ptr (cFromEnum sf)
        {# set StreamParameters.suggestedLatency #} ptr (cFloatConv sl)
        {# set StreamParameters.hostApiSpecificStreamInfo #} ptr ha

-- |Structure used to return information about a host error condition.
data HostErrorInfo = HostErrorInfo {
    hostErrorInfoHostApiType :: HostApiTypeId,
    hostErrorInfoErrorCode :: Int,
    hostErrorInfoErrorText :: String
}
    deriving (Show)

-- |A structure providing information and capabilities of PortAudio devices. Devices may support
-- input, output or both.
data DeviceInfo = DeviceInfo {
    deviceInfoStructVersion :: Int,
    deviceInfoName :: String,
    deviceInfoHostApi :: HostApiIndex,
    deviceInfoMaxInputChannels :: Int,
    deviceInfoMaxOutputChannels :: Int,
    deviceInfoDefaultLowInputLatency :: PaTime,
    deviceInfoDefaultLowOutputLatency :: PaTime,
    deviceInfoDefaultHighInputLatency :: PaTime,
    deviceInfoDefaultHighOutputLatency :: PaTime,
    deviceInfoDefaultSampleRate :: Double
}
    deriving (Show)

-- |Unchanging uniqe identifiers for each supported host API. The values
-- are guaranteed to be unique and will never change. This allows code to
-- be written which conditionally uses host API specific extensions.
{# enum PaHostApiTypeId as HostApiTypeId
    {upcaseFirstLetter} deriving (Show) #}

{# enum PaErrorCode as ErrorCode
    {upcaseFirstLetter} deriving (Show) #}

-- Don't expose this for now, we're not using callbacks.
{# enum PaStreamCallbackResult as StreamCallbackResult
    {upcaseFirstLetter} #}

{# pointer *HostApiInfo as HostApiInfoPtr newtype #}
{# pointer *StreamInfo as StreamInfoPtr newtype #}
{# pointer *StreamParameters as StreamParametersPtr -> StreamParameters #}
nullStreamParameters =  nullPtr :: StreamParametersPtr
{# pointer *DeviceInfo as DeviceInfoPtr newtype #}
{# pointer *HostErrorInfo as HostErrorInfoPtr newtype #}




maybe_get_const_DeviceInfo p@(DeviceInfoPtr ptr) = do
    r <- if (nullPtr == ptr)
           then (return Nothing)
           else do {m <- get_const_DeviceInfo p; return (Just m);}
    return r

maybe_get_const_HostApiInfo p@(HostApiInfoPtr ptr) = do
    r <- if (nullPtr == ptr)
           then (return Nothing)
           else do {m <- get_const_HostApiInfo p; return (Just m);}
    return r

maybe_get_const_HostErrorInfo p@(HostErrorInfoPtr ptr) = do
    r <- if (nullPtr == ptr)
           then (return Nothing)
           else do {m <- get_const_HostErrorInfo p; return (Just m);}
    return r

maybe_get_const_StreamInfo p@(StreamInfoPtr ptr) = do
    r <- if (nullPtr == ptr)
           then (return Nothing)
           else do {m <- get_const_StreamInfo p; return (Just m);}
    return r


get_const_HostErrorInfo (HostErrorInfoPtr ptr) = do
    ht <- liftM cToEnum     $ {# get HostErrorInfo.hostApiType #} ptr
    ec <- liftM cIntConv    $ {# get HostErrorInfo.errorCode   #} ptr
    et <- liftM peekCString $ {# get HostErrorInfo.errorText   #} ptr

    et' <- et

    return $ HostErrorInfo {
        hostErrorInfoHostApiType = ht,
        hostErrorInfoErrorCode = ec,
        hostErrorInfoErrorText = et'
    }

get_const_HostApiInfo (HostApiInfoPtr ptr) = do
    vers <- liftM cIntConv                $ {# get HostApiInfo . structVersion #}     ptr

    --
    -- Note: Since type is a reserved word... I had to do the peek my self.
    -- apiType <- liftM cToEnum           $ {# get HostApiInfo . type #}              ptr
    --
    apiType <- liftM cToEnum              $ (\ptr -> do {peekByteOff ptr 4 :: IO CInt}) ptr

    name' <- liftM peekCString            $ {# get HostApiInfo.name #}                ptr
    deviceCount <- liftM cIntConv         $ {# get HostApiInfo.deviceCount #}         ptr
    defaultInputDevice <- liftM cIntConv  $ {# get HostApiInfo.defaultInputDevice #}  ptr
    defaultOutputDevice <- liftM cIntConv $ {# get HostApiInfo.defaultOutputDevice #} ptr
    name <- name'

    return $ HostApiInfo {
        hostApiInfoStructVersion = vers,
        hostApiInfoApiType = apiType,
        hostApiInfoName = name,
        hostApiInfoDeviceCount = deviceCount,
        hostApiInfoDefaultInputDevice = defaultInputDevice,
        hostApiInfoDefaultOutputDevice = defaultOutputDevice
    }

get_const_DeviceInfo (DeviceInfoPtr ptr) = do
    sv  <- liftM cToEnum $ {# get DeviceInfo.structVersion #} ptr
    nm  <- liftM peekCString $ {# get DeviceInfo.name #} ptr
    ha  <- liftM cIntConv $ {# get DeviceInfo.hostApi #} ptr
    ic  <- liftM cIntConv $ {# get DeviceInfo.maxInputChannels #} ptr
    oc  <- liftM cIntConv $ {# get DeviceInfo.maxOutputChannels #} ptr
    dli <- liftM cFloatConv $ {# get DeviceInfo.defaultLowInputLatency #} ptr
    dlo <- liftM cFloatConv $ {# get DeviceInfo.defaultLowOutputLatency #} ptr
    dhi <- liftM cFloatConv $ {# get DeviceInfo.defaultHighInputLatency #} ptr
    dho <- liftM cFloatConv $ {# get DeviceInfo.defaultHighOutputLatency #} ptr
    dsr <- liftM cFloatConv $ {# get DeviceInfo.defaultSampleRate #} ptr 

    nm' <- nm

    return $ DeviceInfo {
        deviceInfoStructVersion = sv,
        deviceInfoName = nm',
        deviceInfoHostApi = ha,
        deviceInfoMaxInputChannels = ic,
        deviceInfoMaxOutputChannels = oc,
        deviceInfoDefaultLowInputLatency = dli,
        deviceInfoDefaultLowOutputLatency = dlo,
        deviceInfoDefaultHighInputLatency = dhi,
        deviceInfoDefaultHighOutputLatency = dho,
        deviceInfoDefaultSampleRate = dsr
    }

get_const_StreamInfo (StreamInfoPtr ptr) = do
    sv <- liftM cIntConv $ {# get StreamInfo.structVersion #} ptr
    il <- liftM cFloatConv $ {# get StreamInfo.inputLatency #} ptr
    ol <- liftM cFloatConv $ {# get StreamInfo.outputLatency #} ptr
    sr <- liftM cFloatConv $ {# get StreamInfo.sampleRate #} ptr

    return $ StreamInfo {
        streamInfoStructVersion = sv,
        streamInfoInputLatency = il,
        streamInfoOutputLatency = ol,
        streamInfoSampleRate = sr
    }

{-
 - maybeWith :: (a -> (GHC.Ptr.Ptr b -> IO c) -> IO c)
 -     -> Maybe a
 -     -> (GHC.Ptr.Ptr b -> IO c)
 -     -> IO c
 -}
withPA = maybeWith with

peekStream p = do
    ip <- peek p
    return $ PaStream $ ptrToIntPtr ip

-- Only used here. Must not leave.
unPaStream :: PaStream a -> Ptr ()
unPaStream (PaStream ptr) = intPtrToPtr ptr


-- |Translate the supplied PortAudio error code
-- to a textual message.
{# fun Pa_GetErrorText as getErrorText
    { enumToC `ErrorCode' } -> `String' #}

-- |Retrieve the release number of the current PortAudio build.
{# fun pure Pa_GetVersion as getVersion
    {} -> `Int' #}

-- |Retrieve the textual version of the current PortAudio build.
{# fun Pa_GetVersionText as getVersionText
    {} -> `String' #}



{-
 - Foreign Functions
 -}
{# fun Pa_Initialize as initialize_ffi
    {} -> `PaError' cIntConv #}

{# fun Pa_Terminate as terminate_ffi
    {} -> `PaError' cIntConv #}

{# fun Pa_GetHostApiCount as getHostApiCount_ffi
    {} -> `HostApiIndex' cIntConv #}

{# fun Pa_GetDefaultHostApi as getDefaultHostApi_ffi
    {} -> `HostApiIndex' cIntConv #}

{# fun Pa_GetHostApiInfo as getHostApiInfo_ffi
    { cIntConv `HostApiIndex' } -> `Maybe HostApiInfo' maybe_get_const_HostApiInfo* #}

{# fun Pa_HostApiTypeIdToHostApiIndex as hostApiTypeIdToHostApiIndex_ffi
    { enumToC `HostApiTypeId' } -> `HostApiIndex' cIntConv #}

{# fun Pa_HostApiDeviceIndexToDeviceIndex as hostApiDeviceIndexToDeviceIndex_ffi
    { cIntConv `HostApiIndex', `Int' } -> `DeviceIndex' cIntConv #}

-- Since this is a last resort function, we're not going to export it (yet).
{# fun Pa_GetLastHostErrorInfo as getLastHostErrorInfo_ffi
    {} -> `Maybe HostErrorInfo' maybe_get_const_HostErrorInfo* #}

{# fun Pa_GetDeviceCount as getDeviceCount_ffi
    {} -> `DeviceIndex' cIntConv #}

{# fun Pa_GetDefaultInputDevice as getDefaultInputDevice_ffi
    {} -> `DeviceIndex' cIntConv #}

{# fun Pa_GetDefaultOutputDevice as getDefaultOutputDevice_ffi
    {} -> `DeviceIndex' cIntConv #}

{# fun Pa_GetDeviceInfo as getDeviceInfo_ffi
    { cIntConv `DeviceIndex' } -> `Maybe DeviceInfo' maybe_get_const_DeviceInfo* #}

{# fun Pa_IsFormatSupported as isFormatSupported_ffi
    { withPA* `Maybe StreamParameters' ,
      withPA* `Maybe StreamParameters' ,
      `Double' } -> `PaError' cToEnum #}

{- TODO: See what i did with castPtrToFunPtr? That's so wrong it hurts.
 - I applogize for how badly it must hurt you to read the next few lines. -}
{# fun Pa_OpenStream as openStream_ffi
    { alloca- `PaStream a' peekStream*   , 
      withPA* `Maybe StreamParameters' ,
      withPA* `Maybe StreamParameters' ,
      `Double'                           ,
      `Int'                              ,
      `Int'                              ,
      castPtrToFunPtr `Ptr ()'           ,
      id `Ptr ()'
    } -> `PaError' cToEnum #}

{# fun Pa_OpenDefaultStream as openDefaultStream_ffi
    { alloca- `PaStream a' peekStream * ,
      `Int',
      `Int',
      enumToC `SampleFormat',
      `Double',
      `Int',
      castPtrToFunPtr `Ptr ()',
      id `Ptr ()'
    } -> `PaError' cToEnum #}
 
{# fun Pa_CloseStream as closeStream_ffi
    { unPaStream `PaStream a' } -> `PaError' cToEnum #}

{# fun Pa_StartStream as startStream_ffi
    { unPaStream `PaStream a' } -> `PaError' cToEnum #}

{# fun Pa_StopStream as stopStream_ffi
    { unPaStream `PaStream a' } -> `PaError' cToEnum #}

{# fun Pa_AbortStream as abortStream_ffi
    { unPaStream `PaStream a' } -> `PaError' cToEnum #}

{# fun Pa_IsStreamStopped as isStreamStopped_ffi
    { unPaStream `PaStream a' } -> `PaError' cToEnum #}

{# fun Pa_IsStreamActive as isStreamActive_ffi
    { unPaStream `PaStream a' } -> `PaError' cToEnum #}

{# fun Pa_GetStreamInfo as getStreamInfo_ffi
    { unPaStream `PaStream a' } -> `Maybe StreamInfo' maybe_get_const_StreamInfo* #}

{# fun Pa_GetStreamTime as getStreamTime_ffi
    { unPaStream `PaStream a' } -> `PaTime' cFloatConv #}

{# fun Pa_GetStreamCpuLoad as getStreamCpuLoad_ffi
    { unPaStream `PaStream a' } -> `Double' #}

{# fun Pa_ReadStream as readStream_ffi
    { unPaStream `PaStream a' ,
      id `PaStreamBuffer' ,
      `Int' } -> `PaError' cToEnum #}

{# fun Pa_WriteStream as writeStream_ffi
    { unPaStream `PaStream a' ,
      id `PaStreamBuffer',
      `Int' } -> `PaError' cToEnum #}

{# fun Pa_GetStreamReadAvailable as getStreamReadAvailable_ffi
    { unPaStream `PaStream a' } -> `Int' #}

{# fun Pa_GetStreamWriteAvailable as getStreamWriteAvailable_ffi
    { unPaStream `PaStream a' } -> `Int' #}

{# fun Pa_GetSampleSize as getSampleSize_ffi
    { enumToC `SampleFormat' } -> `Int' #} 

{# fun Pa_Sleep as paSleep_ffi
    { `Int' } -> `()' #}

