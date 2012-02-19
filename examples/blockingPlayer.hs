{-# LANGUAGE ScopedTypeVariables #-}

import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as VSF
import qualified Data.Vector.Storable as V
import System.Environment (getArgs)

import Foreign.C.Types
import Foreign.Storable

import Sound.PortAudio.Base
import Sound.PortAudio

import Control.Concurrent
import Control.Applicative

main :: IO ()
main = do
    [inFile] <- getArgs
    (info, Just (x :: VSF.Buffer Float)) <- SF.readFile inFile
    
    let vecData = V.map (\i -> (realToFrac i) :: CFloat ) VSF.fromBuffer x
    fileData <- newMVar vecData    
    putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
    putStrLn $ "channels: "    ++ (show $ SF.channels info)
    putStrLn $ "frames: "      ++ (show $ SF.frames info)
    putStrLn $ "dataLen: "     ++ (show $ V.length vecData)

    result <- withPortAudio $ do
        withDefaultOutputInfo $ \(out, outInfo) -> do
            songDone <- newEmptyMVar

            let strmParams = Just $ StreamParameters out (fromIntegral $ SF.channels info) (defaultHighOutputLatency outInfo)
                smpRate = realToFrac $ SF.samplerate info
                frmPerBuf = Just $ fromIntegral framesPerBuffer
                framesPerBuffer = 2000


            withStream Nothing strmParams smpRate frmPerBuf [ClipOff, PrimeOutputBuffers] Nothing $ \strm -> do
                s1 <- addStreamFin (makeFinishedCallback $ streamFinished "Finished Playing Song!") strm
                s2 <- startStream strm
                takeMVar songDone
                s3 <- stopStream strm
                return $ Right ()

    case result of
        Left err -> print err
        Right _ -> return ()