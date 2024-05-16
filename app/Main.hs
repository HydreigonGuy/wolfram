module Main where

import Lib
import System.Environment
import System.Exit ( ExitCode(ExitFailure), exitWith )

main :: IO ()
main = do
        args <- getArgs
        case length args of
            0 -> exitWith (ExitFailure 84)
            _ -> case getOpts defaultConf args of
                Nothing -> exitWith (ExitFailure 84)
                Just a -> runWolfram a (createStartArray a)
