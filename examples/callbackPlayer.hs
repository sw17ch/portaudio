{-# LANGUAGE ScopedTypeVariables #-}

-- This is how you would play a song with libsnd and portaudio with Haskell using Callbacks.
-- Because Haskell is GC, it is not recommended you use callbacks, this is simply for demo purposes
-- You should use blocking IO instead! Also never test code on with headphones or speakers on loud volume!

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

streamFinished :: String -> IO ()
streamFinished msg = putStrLn ("Stream Completed: " ++ msg)

simpleCallback :: MVar (V.Vector Float) -> MVar () -> StreamCallback CFloat CFloat
simpleCallback dataMVar doneMVar _ _ frames _ out = do
    pnts <- modifyMVar dataMVar (\vec -> let (x, y) = V.splitAt (fromIntegral frames) vec in return (y, x))
    if (V.null pnts)
        then (putMVar doneMVar () >> return Complete)
        else do
            V.foldM'_ (\i e -> pokeElemOff out i (realToFrac e) >> return (i + 1)) 0 pnts
            return Continue


main :: IO ()
main = do
    [inFile] <- getArgs
    (info, Just (x :: VSF.Buffer Float)) <- SF.readFile inFile
    
    let vecData = VSF.fromBuffer x
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
                callb = Just $ simpleCallback fileData songDone
                framesPerBuffer = 2000


            withStream Nothing strmParams smpRate frmPerBuf [ClipOff, PrimeOutputBuffers] callb $ \strm -> do
                s1 <- addStreamFin (makeFinishedCallback $ streamFinished "Finished Playing Song!") strm
                s2 <- startStream strm
                takeMVar songDone
                s3 <- stopStream strm
                return $ Right ()

    case result of
        Left err -> print err
        Right _ -> return ()