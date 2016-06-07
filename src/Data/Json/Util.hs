{-# LANGUAGE OverloadedStrings #-}

module Data.Json.Util where

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Array, Object), decodeStrict)
import           Data.ByteString     as BS (hGetContents)
import           Data.HashMap.Strict as HM (toList)
import           Data.Text           as T (Text)
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)

type Path                   = [Text]
type PropertyName           =  Text
type PathsPropertyNameValue = [(Path, (PropertyName, Value))]

readJson :: FilePath -> IO (Maybe Value)
readJson filename =
    withFile filename ReadMode (BS.hGetContents >=> return . decodeStrict)

findInJson :: Text -> Value -> PathsPropertyNameValue
findInJson goal top = fVal top []
  where
    fVal :: Value -> Path -> PathsPropertyNameValue
    fVal (Object o) path   = fObj (HM.toList o)
      where
        fObj []            = []
        fObj (hd@(k,v):tl)
            | goal == k    = (P.reverse path, hd) : fObj tl
            | otherwise    = fVal v (k:path)     ++ fObj tl
    fVal (Array  a) path   = P.concatMap (`fVal` path) a
    fVal         _     _   = []
