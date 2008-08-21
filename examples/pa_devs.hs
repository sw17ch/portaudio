module Main where

{- Modeled from pa_devs.c in the PortAudio project. -}

import Sound.PortAudio

main = do
    r <- withPortAudio paActions
    return ()

paActions = do
    let pa_vers = getVersion
    pa_vers_text <- getVersionText

    putStrLn $ "PortAudio version number = " ++ (show pa_vers)
    putStrLn $ "PortAudio version text = " ++ pa_vers_text

    num_devs <- getDeviceCount
    case num_devs of
         (Right n) -> do putStrLn $ "Number of devices = " ++ (show n)
                         mapM_ displayDevice [0..(n - 1)]
         (Left  e) -> do putStrLn $ "ERROR: getDeviceCount returned " ++ (show e)

displayDevice d = do
    putStrLn $ "--------------------------------------- device #" ++ (show d)
    (Just di) <- getDeviceInfo d
    (Just def_input) <- getDefaultInputDevice
    (Just def_output) <- getDefaultOutputDevice
    (Just api_info)  <- getHostApiInfo (deviceInfoHostApi di)

    let input_txt = if (d == def_input)
                       then "Default Input"
                       else if (d == (hostApiInfoDefaultInputDevice api_info))
                               then concat ["Default ",(hostApiInfoName api_info)," Input"]
                               else ""

    let output_txt = if (d == def_output)
                        then "Default Output"
                        else if (d == (hostApiInfoDefaultOutputDevice api_info))
                                then concat ["Default ",(hostApiInfoName api_info)," Output"]
                                else ""

    let def_txt = concat ["[ ", input_txt, " , ", output_txt, " ]"]
    if (length def_txt > 7)
       then putStrLn def_txt
       else return ()
    
    putStrLn $ "Name = " ++ deviceInfoName di
    putStrLn $ "Host API = " ++ (hostApiInfoName api_info)
    putStrLn $ concat ["Max Inputs = ", (show $ deviceInfoMaxInputChannels di),
                       ", Max Outputs = ", (show $ deviceInfoMaxOutputChannels di)]
    putStrLn $ "Default low input latency = " ++ (show $ deviceInfoDefaultLowInputLatency di)
    putStrLn $ "Default low output latency = " ++ (show $ deviceInfoDefaultLowOutputLatency di)
    putStrLn $ "Default high input latency = " ++ (show $ deviceInfoDefaultHighInputLatency di)
    putStrLn $ "Default high output latency = " ++ (show $ deviceInfoDefaultHighOutputLatency di)
    putStrLn $ "Default Sample Rate = " ++ (show $ deviceInfoDefaultSampleRate di)

    let id_params = StreamParameters {
        streamParametersDevice = d,
        streamParametersChannelCount = deviceInfoMaxInputChannels di,
        streamParametersSampleFormat = PaInt16,
        streamParametersSuggestedLatency = 0,
        streamParametersHostApiSpecificStreamInfo = paNullPtr 
    }
    let od_params = StreamParameters {
        streamParametersDevice = d,
        streamParametersChannelCount = deviceInfoMaxOutputChannels di,
        streamParametersSampleFormat = PaInt16,
        streamParametersSuggestedLatency = 0,
        streamParametersHostApiSpecificStreamInfo = paNullPtr 
    }

    if (streamParametersChannelCount id_params > 0)
       then do putStrLn "Sample Rates -- Half-Duplex 16 bit -- Input"
               printSupportedStandardSampleRates (Just id_params) Nothing 
       else return ()
    
    if (streamParametersChannelCount od_params > 0)
       then do putStrLn "Sample Rates -- Half-Duplex 16 bit -- Output"
               printSupportedStandardSampleRates Nothing (Just od_params) 
       else return ()

    if (and [streamParametersChannelCount id_params > 0, streamParametersChannelCount od_params > 0])
       then do putStrLn "Sample Rates -- Half-Duplex 16 bit"
               printSupportedStandardSampleRates (Just id_params) (Just od_params) 
       else return ()

printSupportedStandardSampleRates ip op = do
    mapM_ (checkFmt ip op) standardSampleRates
    putStrLn ""
    where checkFmt ip op sr = do
            r <- isFormatSupported ip op sr
            case r of
                 (Right _) -> putStr $ (show sr) ++ " "
                 (Left _) -> return ()

    
