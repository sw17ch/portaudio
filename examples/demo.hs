module Main where

import Sound.PortAudio.Base

main :: IO ()
main = do
    pa_Initialize >>= print
