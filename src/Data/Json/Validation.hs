{-# LANGUAGE OverloadedStrings #-}

module Data.Json.Validation where

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Array, Object), decodeStrict)
import           Data.ByteString     as BS (hGetContents)
import           Data.HashMap.Strict as HM (toList)
import           Data.Text           as T (Text)
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)

readJson :: FilePath -> IO (Maybe Value)
readJson filename =
    withFile filename ReadMode (BS.hGetContents >=> return . decodeStrict)

--                       path   field  value
type PathFieldValue = [([Text], (Text, Value))]

findInJson :: Text -> Value -> PathFieldValue
findInJson goal top = fJson top []
  where
    --                 path
    fJson :: Value -> [Text] -> PathFieldValue
    fJson (Object o) path  = fObj (HM.toList o)
      where
        fObj []            = []
        fObj (hd@(k,v):tl)
            | goal == k    = (P.reverse path, hd) :  fObj tl
            | otherwise    = fJson v (k:path)     ++ fObj tl
    fJson (Array  a) path  = P.concatMap (`fJson` path) a
    fJson         _     _  = []
