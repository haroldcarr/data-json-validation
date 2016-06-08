{-# LANGUAGE OverloadedStrings #-}

module Data.Json.Util where

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Array, Object), decodeStrict)
import           Data.ByteString     as BS (hGetContents)
import           Data.HashMap.Strict as HM (toList)
import           Data.Text           as T (Text)
import           Data.Vector         as V (imap)
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)

data PathSegment            = P Text | VRef Int deriving Eq
instance Show PathSegment where
    show (P    a) =            show a
    show (VRef a) = "VRef " ++ show a
type Path                   = [PathSegment]
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
            | otherwise    = fVal v (P k:path)   ++ fObj tl
    fVal (Array  a) path   = P.concatMap (\(vr,av) -> fVal av (vr:path))
                                         (imap (\i av -> (VRef i, av)) a)
    fVal         _     _   = []
