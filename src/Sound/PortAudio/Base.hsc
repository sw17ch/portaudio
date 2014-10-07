{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving, EmptyDataDecls #-}
module Sound.PortAudio.Base where

#include <portaudio.h>

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

{- Integral types. -}
newtype PaErrorCode = PaErrorCode { unPaErrorCode :: CInt }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaDeviceIndex = PaDeviceIndex { unPaDeviceIndex :: CInt }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaHostApiIndex = PaHostApiIndex { unPaHostApiIndex :: CInt }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaHostApiTypeId = PaHostApiTypeId { unPaHostApiTypeId :: CInt }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaSampleFormat = PaSampleFormat { unPaSampleFormat :: CULong }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaStreamFlags = PaStreamFlags { unPaStreamFlags :: CULong }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaStreamCallbackFlags = PaStreamCallbackFlags { unPaStreamCallbackFlags :: CULong }
    deriving (Eq, Show, Num, Enum, Storable)

newtype PaStreamCallbackResult = PaStreamCallbackResult { unPaStreamCallbackResult :: CInt }
    deriving (Eq, Show, Num, Enum, Storable)

{- FunctionPtr Types -}
type PaStreamCallback =
    (  Ptr () -- input
    -> Ptr () -- output
    -> CULong  -- frameCount
    -> Ptr PaStreamCallbackTimeInfo -- timeInfo
    -> CInt -- statusFlags
    -> Ptr () -- userData
    -> IO CInt
    )
type PaStreamCallbackFunPtr = FunPtr PaStreamCallback

type PaStreamFinishedCallback =
    (  Ptr () -> IO ()
    )  

type PaStreamFinishedCallbackFunPtr = FunPtr PaStreamFinishedCallback

{- Other Types -}
newtype PaTime = PaTime { unPaTime :: CDouble }
    deriving (Eq, Show, Storable)

data PaStream

{- Structures -}
data PaHostApiInfo = PaHostApiInfo {
    structVersion_PaHostApiInfo :: CInt,
    hostapitype :: PaHostApiTypeId,
    name_PaHostApiInfo :: String,
    deviceCount :: CInt,
    defaultInputDevice :: PaDeviceIndex,
    defaultOutputDevice :: PaDeviceIndex
} deriving (Show)

data PaHostErrorInfo = PaHostErrorInfo {
    hostApiType :: PaHostApiTypeId,
    errorCode :: CLong,
    errorText :: String
} deriving (Show)

data PaDeviceInfo = PaDeviceInfo {
    structVersion_PaDeviceInfo :: CInt,
    name_PaDeviceInfo :: String,
    hostApi :: PaHostApiIndex,
    maxInputChannels :: CInt,
    maxOutputChannels :: CInt,
    defaultLowInputLatency :: PaTime,
    defaultLowOutputLatency :: PaTime,
    defaultHighInputLatency :: PaTime,
    defaultHighOutputLatency :: PaTime,
    defaultSampleRate :: CDouble
} deriving (Show)

data PaStreamParameters = PaStreamParameters {
    device :: PaDeviceIndex,
    channelCount :: CInt,
    sampleFormat :: PaSampleFormat,
    suggestedLatency :: PaTime,
    hostApiSpecificStreamInfo :: Ptr ()
} deriving (Show)

data PaStreamCallbackTimeInfo = PaStreamCallbackTimeInfo {
    inputBufferAdcTime :: PaTime,
    currentTime :: PaTime,
    outputBufferDacTime :: PaTime
} deriving (Show)

data PaStreamInfo = PaStreamInfo {
    structVersion_PaStreamInfo :: CInt,
    inputLatency :: PaTime,
    outputLatency :: PaTime,
    sampleRate :: CDouble
} deriving (Show)

{- Enumerable values -}
#{enum PaErrorCode, PaErrorCode
    , paNoError = paNoError
    , paNotInitialized = paNotInitialized 
    , paUnanticipatedHostError = paUnanticipatedHostError 
    , paInvalidChannelCount = paInvalidChannelCount 
    , paInvalidSampleRate = paInvalidSampleRate 
    , paInvalidDevice = paInvalidDevice 
    , paInvalidFlag = paInvalidFlag 
    , paSampleFormatNotSupported = paSampleFormatNotSupported 
    , paBadIODeviceCombination = paBadIODeviceCombination 
    , paInsufficientMemory = paInsufficientMemory 
    , paBufferTooBig = paBufferTooBig 
    , paBufferTooSmall = paBufferTooSmall 
    , paNullCallback = paNullCallback 
    , paBadStreamPtr = paBadStreamPtr 
    , paTimedOut = paTimedOut 
    , paInternalError = paInternalError 
    , paDeviceUnavailable = paDeviceUnavailable 
    , paIncompatibleHostApiSpecificStreamInfo = paIncompatibleHostApiSpecificStreamInfo 
    , paStreamIsStopped = paStreamIsStopped 
    , paStreamIsNotStopped = paStreamIsNotStopped 
    , paInputOverflowed = paInputOverflowed 
    , paOutputUnderflowed = paOutputUnderflowed 
    , paHostApiNotFound = paHostApiNotFound 
    , paInvalidHostApi = paInvalidHostApi 
    , paCanNotReadFromACallbackStream = paCanNotReadFromACallbackStream 
    , paCanNotWriteToACallbackStream = paCanNotWriteToACallbackStream 
    , paCanNotReadFromAnOutputOnlyStream = paCanNotReadFromAnOutputOnlyStream 
    , paCanNotWriteToAnInputOnlyStream = paCanNotWriteToAnInputOnlyStream 
    , paIncompatibleStreamHostApi = paIncompatibleStreamHostApi 
    , paBadBufferPtr = paBadBufferPtr 
    }

#{enum PaHostApiTypeId, PaHostApiTypeId
    , paInDevelopment = paInDevelopment 
    , paDirectSound = paDirectSound 
    , paMME = paMME 
    , paASIO = paASIO 
    , paSoundManager = paSoundManager 
    , paCoreAudio = paCoreAudio 
    , paOSS = paOSS 
    , paALSA = paALSA 
    , paAL = paAL 
    , paBeOS = paBeOS 
    , paWDMKS = paWDMKS 
    , paJACK = paJACK 
    , paWASAPI = paWASAPI 
    , paAudioScienceHPI = paAudioScienceHPI 
    }

#{enum PaDeviceIndex, PaDeviceIndex
    , paNoDevice = paNoDevice
    , paUseHostApiSpecificDeviceSpecification = paUseHostApiSpecificDeviceSpecification
    }

#{enum PaSampleFormat, PaSampleFormat
    , paFloat32 = paFloat32
    , paInt32 = paInt32
    , paInt24 = paInt24
    , paInt16 = paInt16
    , paInt8 = paInt8
    , paUInt8 = paUInt8
    , paCustomFormat = paCustomFormat
    , paNonInterleaved = paNonInterleaved
    }

#{enum PaStreamFlags, PaStreamFlags
    , paNoFlag = paNoFlag
    , paClipOff = paClipOff
    , paDitherOff = paDitherOff
    , paNeverDropInput = paNeverDropInput
    , paPrimeOutputBuffersUsingStreamCallback = paPrimeOutputBuffersUsingStreamCallback
    , paPlatformSpecificFlags = paPlatformSpecificFlags
    }

#{enum PaStreamCallbackFlags, PaStreamCallbackFlags
    , paInputUnderflow = paInputUnderflow
    , paInputOverflow = paInputOverflow
    , paOutputUnderflow = paOutputUnderflow
    , paOutputOverflow = paOutputOverflow
    , paPrimingOutput = paPrimingOutput
    }

#{enum PaStreamCallbackResult, PaStreamCallbackResult
    , paContinue = paContinue
    , paComplete = paComplete
    , paAbort = paAbort
    }



{- Other special static values. -}
paFormatIsSupported :: PaErrorCode
paFormatIsSupported = PaErrorCode #{const paFormatIsSupported}

paFramesPerBufferUnspecified :: CULong
paFramesPerBufferUnspecified = #{const paFramesPerBufferUnspecified}

{- Functions -}

{- int Pa_GetVersion( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetVersion"
    pa_GetVersion :: IO CInt

{- const char * Pa_GetVersionText( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetVersionText"
    pa_GetVersionText :: IO CString

{- const char * Pa_GetErrorText( PaError errorCode ); -}
foreign import ccall unsafe "portaudio.h Pa_GetErrorText"
    pa_GetErrorText :: CInt -> IO CString 

{- PaError Pa_Initialize( void ); -}
foreign import ccall unsafe "portaudio.h Pa_Initialize"
    pa_Initialize :: IO CInt

{- PaError Pa_Terminate( void ); -}
foreign import ccall unsafe "portaudio.h Pa_Terminate"
    pa_Terminate :: IO CInt

{- PaHostApiIndex Pa_GetHostApiCount( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetHostApiCount"
    pa_GetHostApiCount :: IO CInt

{- PaHostApiIndex Pa_GetDefaultHostApi( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetDefaultHostApi"
    pa_GetDefaultHostApi :: IO CInt

{- const PaHostApiInfo * Pa_GetHostApiInfo( PaHostApiIndex hostApi ); -}
foreign import ccall unsafe "portaudio.h Pa_GetHostApiInfo"
    pa_GetHostApiInfo :: CInt -> IO (Ptr PaHostApiInfo)

{- PaHostApiIndex Pa_HostApiTypeIdToHostApiIndex( PaHostApiTypeId type ); -}
foreign import ccall unsafe "portaudio.h Pa_HostApiTypeIdToHostApiIndex"
    pa_HostApiTypeIdToHostApiIndex :: CInt -> CInt

{- PaDeviceIndex Pa_HostApiDeviceIndexToDeviceIndex( PaHostApiIndex hostApi, int hostApiDeviceIndex ); -}
foreign import ccall unsafe "portaudio.h Pa_HostApiDeviceIndexToDeviceIndex"
    pa_HostApiDeviceIndexToDeviceIndex :: CInt -> CInt -> CInt

{- const PaHostErrorInfo* Pa_GetLastHostErrorInfo( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetLastHostErrorInfo"
    pa_GetLastHostErrorInfo :: IO (Ptr PaHostErrorInfo)

{- PaDeviceIndex Pa_GetDeviceCount( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetDeviceCount"
    pa_GetDeviceCount :: IO CInt

{- PaDeviceIndex Pa_GetDefaultInputDevice( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetDefaultInputDevice"
    pa_GetDefaultInputDevice :: IO CInt

{- PaDeviceIndex Pa_GetDefaultOutputDevice( void ); -}
foreign import ccall unsafe "portaudio.h Pa_GetDefaultOutputDevice"
    pa_GetDefaultOutputDevice :: IO CInt

{- const PaDeviceInfo* Pa_GetDeviceInfo( PaDeviceIndex device ); -}
foreign import ccall unsafe "portaudio.h Pa_GetDeviceInfo"
    pa_GetDeviceInfo :: CInt -> IO (Ptr PaDeviceInfo)

{- PaError Pa_IsFormatSupported( const PaStreamParameters *inputParameters,
                                 const PaStreamParameters *outputParameters,
                                 double sampleRate ); -}
foreign import ccall unsafe "portaudio.h Pa_IsFormatSupported"
    pa_IsFormatSupported :: Ptr PaStreamParameters
                         -> Ptr PaStreamParameters
                         -> IO CDouble

{- PaError Pa_OpenStream( PaStream** stream,
                          const PaStreamParameters *inputParameters,
                          const PaStreamParameters *outputParameters,
                          double sampleRate,
                          unsigned long framesPerBuffer,
                          PaStreamFlags streamFlags,
                          PaStreamCallback *streamCallback,
                          void *userData ); -}
foreign import ccall safe "portaudio.h Pa_OpenStream"
    pa_OpenStream :: Ptr (Ptr PaStream)
                  -> Ptr PaStreamParameters
                  -> Ptr PaStreamParameters
                  -> CDouble
                  -> CULong
                  -> PaStreamFlags
                  -> PaStreamCallbackFunPtr
                  -> Ptr ()
                  -> IO CInt

{- PaError Pa_OpenDefaultStream( PaStream** stream,
                                 int numInputChannels,
                                 int numOutputChannels,
                                 PaSampleFormat sampleFormat,
                                 double sampleRate,
                                 unsigned long framesPerBuffer,
                                 PaStreamCallback *streamCallback,
                                 void *userData ); -}
foreign import ccall safe "portaudio.h Pa_OpenDefaultStream"
    pa_OpenDefaultStream :: Ptr (Ptr PaStream)
                         -> CInt
                         -> CInt
                         -> PaSampleFormat
                         -> CDouble
                         -> CULong
                         -> PaStreamCallbackFunPtr
                         -> Ptr ()
                         -> IO CInt

{- PaError Pa_CloseStream( PaStream *stream ); -}
foreign import ccall unsafe "portaudio.h Pa_CloseStream"
    pa_CloseStream :: Ptr PaStream
                   -> IO CInt

{- PaError Pa_SetStreamFinishedCallback( PaStream *stream, PaStreamFinishedCallback* streamFinishedCallback );  -}
foreign import ccall safe "portaudio.h Pa_SetStreamFinishedCallback"
    pa_SetStreamFinishedCallback :: Ptr PaStream
                                 -> PaStreamFinishedCallbackFunPtr
                                 -> IO CInt

{- PaError Pa_StartStream( PaStream *stream ); -}
foreign import ccall safe "portaudio.h Pa_StartStream"
    pa_StartStream :: Ptr PaStream
                   -> IO CInt

{- PaError Pa_StopStream( PaStream *stream ); -}
foreign import ccall safe "portaudio.h Pa_StopStream"
    pa_StopStream :: Ptr PaStream
                  -> IO CInt

{- PaError Pa_AbortStream( PaStream *stream ); -}
foreign import ccall safe "portaudio.h Pa_AbortStream"
    pa_AbortStream :: Ptr PaStream
                   -> IO CInt

{- PaError Pa_IsStreamStopped( PaStream *stream ); -}
foreign import ccall unsafe "portaudio.h Pa_IsStreamStopped"
    pa_IsStreamStopped :: Ptr PaStream
                       -> IO CInt

{- PaError Pa_IsStreamActive( PaStream *stream ); -}
foreign import ccall unsafe "portaudio.h Pa_IsStreamActive"
    pa_IsStreamActive :: Ptr PaStream
                       -> IO CInt

{- const PaStreamInfo* Pa_GetStreamInfo( PaStream *stream ); -}
foreign import ccall unsafe "portaudio.h Pa_GetStreamInfo"
    pa_GetStreamInfo :: Ptr PaStream
                     -> IO (Ptr PaStreamInfo)

{- PaTime Pa_GetStreamTime( PaStream *stream ); -}
foreign import ccall unsafe "portaudio.h Pa_GetStreamTime"
    pa_GetStreamTime :: Ptr PaStream
                     -> IO PaTime

{- double Pa_GetStreamCpuLoad( PaStream* stream ); -}
foreign import ccall unsafe "portaudio.h Pa_GetStreamCpuLoad"
    pa_GetStreamCpuLoad :: Ptr PaStream
                        -> IO CDouble

{- PaError Pa_ReadStream( PaStream* stream,
                          void *buffer,
                          unsigned long frames ); -}
foreign import ccall unsafe "portaudio.h Pa_ReadStream"
    pa_ReadStream :: Ptr PaStream
                  -> Ptr ()
                  -> CULong
                  -> IO CInt

{- PaError Pa_WriteStream( PaStream* stream,
                           const void *buffer,
                           unsigned long frames ); -}
foreign import ccall unsafe "portaudio.h Pa_WriteStream"
    pa_WriteStream :: Ptr PaStream
                   -> Ptr ()
                   -> CULong
                   -> IO CInt

{- signed long Pa_GetStreamReadAvailable( PaStream* stream ); -}
foreign import ccall unsafe "portaudio.h Pa_GetStreamReadAvailable"
    pa_GetStreamReadAvailable :: Ptr PaStream
                              -> IO CLong

{- signed long Pa_GetStreamWriteAvailable( PaStream* stream ); -}
foreign import ccall unsafe "portaudio.h Pa_GetStreamWriteAvailable"
    pa_GetStreamWriteAvailable :: Ptr PaStream
                               -> IO CLong

{- PaError Pa_GetSampleSize( PaSampleFormat format ); -}
foreign import ccall unsafe "portaudio.h Pa_GetSampleSize"
    pa_GetSampleSize :: PaSampleFormat
                     -> IO CInt

{- void Pa_Sleep( long msec ); -}
foreign import ccall unsafe "portaudio.h Pa_Sleep"
    pa_Sleep :: CLong
             -> IO ()

{- Storable Instances for Structures -}
instance Storable PaHostApiInfo where
    sizeOf _ = #{const sizeof(PaHostApiInfo)}
    alignment _ = #{const __alignof__(PaHostApiInfo)}
    peek p = do
        sv <- #{peek PaHostApiInfo, structVersion} p
        ty <- #{peek PaHostApiInfo, type} p
        nm <- (peek $ #{ptr  PaHostApiInfo, name} p) >>= peekCString
        dc <- #{peek PaHostApiInfo, deviceCount} p
        di <- #{peek PaHostApiInfo, defaultInputDevice} p
        dd <- #{peek PaHostApiInfo, defaultOutputDevice} p
        return $ PaHostApiInfo {
            structVersion_PaHostApiInfo = sv,
            hostapitype = ty,
            name_PaHostApiInfo = nm,
            deviceCount = dc,
            defaultInputDevice = di,
            defaultOutputDevice = dd
        }
    poke _ _ = error "Bad user! You shouldn't be poking PaHostApiInfo's!"

instance Storable PaHostErrorInfo where
    sizeOf _ = #{const sizeof(PaHostErrorInfo)}
    alignment _ = #{const __alignof__(PaHostErrorInfo)}
    peek p = do
        ha <- #{peek PaHostErrorInfo, hostApiType} p
        ec <- #{peek PaHostErrorInfo, errorCode} p
        ep <- return $ #{ptr PaHostErrorInfo, errorText} p
        et <- peekCString ep
        return $ PaHostErrorInfo {
            hostApiType = ha,
            errorCode = ec,
            errorText = et
        }
    poke _ _ = error "Bad user! You shouldn't be poking PaHostErrorInfo's!"

instance Storable PaDeviceInfo where
    sizeOf _ = #{const sizeof(PaDeviceInfo)}
    alignment _ = #{const __alignof__(PaDeviceInfo)}
    peek p = do
        sv <- #{peek PaDeviceInfo, structVersion} p
        nm <- (peek $ #{ptr  PaDeviceInfo, name} p) >>= peekCString
        ha <- #{peek PaDeviceInfo, hostApi} p
        mi <- #{peek PaDeviceInfo, maxInputChannels} p
        mo <- #{peek PaDeviceInfo, maxOutputChannels} p
        li <- #{peek PaDeviceInfo, defaultLowInputLatency} p
        lo <- #{peek PaDeviceInfo, defaultLowOutputLatency} p
        hi <- #{peek PaDeviceInfo, defaultHighInputLatency} p
        ho <- #{peek PaDeviceInfo, defaultHighOutputLatency} p
        ds <- #{peek PaDeviceInfo, defaultSampleRate} p
        return $ PaDeviceInfo {
            structVersion_PaDeviceInfo = sv,
            name_PaDeviceInfo = nm,
            hostApi = ha,
            maxInputChannels = mi,
            maxOutputChannels = mo,
            defaultLowInputLatency = li,
            defaultLowOutputLatency = lo,
            defaultHighInputLatency = hi,
            defaultHighOutputLatency = ho,
            defaultSampleRate = ds
        }
    poke _ _ = error "Bad user! You shouldn't be poking PaDeviceInfo's!"

instance Storable PaStreamParameters where
    sizeOf _ = #{const sizeof(PaStreamParameters)}
    alignment _ = #{const __alignof__(PaStreamParameters)}
    peek p = do
        de <- #{peek PaStreamParameters, device} p
        cc <- #{peek PaStreamParameters, channelCount} p
        sf <- #{peek PaStreamParameters, sampleFormat} p
        sl <- #{peek PaStreamParameters, suggestedLatency} p
        si <- #{peek PaStreamParameters, hostApiSpecificStreamInfo} p
        return $ PaStreamParameters {
            device = de,
            channelCount = cc,
            sampleFormat = sf,
            suggestedLatency = sl,
            hostApiSpecificStreamInfo = si
        }
    poke p v = do
        #{poke PaStreamParameters, device} p (device v)
        #{poke PaStreamParameters, channelCount} p (channelCount v)
        #{poke PaStreamParameters, sampleFormat} p (sampleFormat v)
        #{poke PaStreamParameters, suggestedLatency} p (suggestedLatency v)
        #{poke PaStreamParameters, hostApiSpecificStreamInfo} p (hostApiSpecificStreamInfo v)

instance Storable PaStreamCallbackTimeInfo where
    sizeOf _ = #{const sizeof(PaStreamCallbackTimeInfo)}
    alignment _ = #{const __alignof__(PaStreamCallbackTimeInfo)}
    peek p = do
        at <- #{peek PaStreamCallbackTimeInfo, inputBufferAdcTime} p
        ct <- #{peek PaStreamCallbackTimeInfo, currentTime} p
        dt <- #{peek PaStreamCallbackTimeInfo, outputBufferDacTime} p
        return $ PaStreamCallbackTimeInfo {
            inputBufferAdcTime = at,
            currentTime = ct,
            outputBufferDacTime = dt
        }
    poke p v = do -- not sure if this should be allowed.
        #{poke PaStreamCallbackTimeInfo, inputBufferAdcTime} p (inputBufferAdcTime v)
        #{poke PaStreamCallbackTimeInfo, currentTime} p (currentTime v)
        #{poke PaStreamCallbackTimeInfo, outputBufferDacTime} p (outputBufferDacTime v)

instance Storable PaStreamInfo where
    sizeOf _ = #{const sizeof(PaStreamInfo)}
    alignment _ = #{const __alignof__(PaStreamInfo)}
    peek p = do
        sv <- #{peek PaStreamInfo, structVersion} p
        il <- #{peek PaStreamInfo, inputLatency} p
        ol <- #{peek PaStreamInfo, outputLatency} p
        sl <- #{peek PaStreamInfo, sampleRate} p
        return $ PaStreamInfo {
            structVersion_PaStreamInfo = sv,
            inputLatency = il,
            outputLatency = ol,
            sampleRate = sl
        }
    poke p v = do -- not sure if this should be allowed.
        #{poke PaStreamInfo, structVersion} p (structVersion_PaStreamInfo v)
        #{poke PaStreamInfo, inputLatency} p (inputLatency v)
        #{poke PaStreamInfo, outputLatency} p (outputLatency v)
        #{poke PaStreamInfo, sampleRate} p (sampleRate v)


{- Callback Wrappers -}
foreign import ccall safe "wrapper"
    wrap_PaStreamFinishedCallback :: PaStreamFinishedCallback -> IO PaStreamFinishedCallbackFunPtr
foreign import ccall safe "wrapper"
    wrap_PaStreamCallback :: PaStreamCallback -> IO PaStreamCallbackFunPtr
