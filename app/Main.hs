{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.GraphViz           as GV hiding (toDot)
import           Data.Json.Validation
import           Data.Json.Visualization
import           System.Environment

main :: IO ()
main = do
    as <- getArgs
    case as of
      inFilename:outFilename:_ -> do
          r <- readJson inFilename
          _ <- case r of
            Nothing -> error $ "could not read " ++ inFilename
            Just v  -> let d = (toDot . findInJson "$ref") v
                       in runGraphviz d Png outFilename
          return ()
      _ -> error "Usage: vjson <input filename> <output filename>"
