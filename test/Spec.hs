{-# LANGUAGE OverloadedStrings #-}

module Spec where

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Object, String))
import           Data.HashMap.Strict as HM (fromList)
import           Data.Json.Util
import           Data.Text           as T (Text)
import           System.Directory    (doesFileExist)
import           System.IO.Unsafe    (unsafePerformIO)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import           Test.HUnit.Util     as U (t)

test :: IO Counts
test = runTestTT $ TestList $ ts1 ++ ts2 ++ ts3 ++ ts4

iRead :: FilePath -> IO FilePath
iRead filename = do
    e <- doesFileExist filename
    return $ if e then filename else "test/" ++ filename

readFind :: Text -> FilePath -> IO PathsPropertyNameValue
readFind goal filename = iRead filename >>= (readJson >=> (\(Just s) -> return $ findInJson goal s))

ts1 :: [Test]
ts1 = U.t "ts1"
    (unsafePerformIO (readFind "$ref" "refs-simple-invalid.json"))
    [([P "definitions",P "Parent"]
     ,("$ref",String "#/definitions/DoesNotExist"))
    ,([P "definitions",P "Pet",P "properties",P "tags",P "items"]
     ,("$ref",String "#/definitions/Tag"))
    ,([P "paths",P "/pets",P "get",P "responses",P "200",P "schema",P "items"]
     ,("$ref",String "#/definitions/Pet"))]

ts2 :: [Test]
ts2 = U.t "ts2"
    (unsafePerformIO (readFind "$ref" "refs-indirect-circular-ancestor-invalid.json"))
    [([P "definitions",P "Parent",P "allOf", VRef 0]
     ,("$ref",String "#/definitions/Circular2"))
    ,([P "definitions",P "Circular1",P "allOf", VRef 0]
     ,("$ref",String "#/definitions/Parent"))
    ,([P "definitions",P "Circular2",P "allOf", VRef 0]
     ,("$ref",String "#/definitions/Circular1"))
    ,([P "definitions",P "Pet",P "properties",P "category"]
     ,("$ref",String "#/definitions/Category"))
    ,([P "definitions",P "Pet",P "properties",P "tags",P "items"]
     ,("$ref",String "#/definitions/Tag"))
    ,([P "paths",P "/pets",P "get",P "responses",P "200",P "schema",P "items"]
     ,("$ref",String "#/definitions/Pet"))]

ts3 :: [Test]
ts3 = U.t "ts3"
    (unsafePerformIO (readFind "X" "x.json"))
    [([P "Tag"]
     ,("X",Object (fromList [("Tag-X-value",Object (fromList [("X",String "Tag-X-value-X-value")]))])))
    ,([P "responses",P "default"]
     ,("X",String "responses-default-X-value"))
    ,([P "responses",P "200",P "schema",P "items"]
     ,("X",String "responses-200-schema-items-X-value"))
    ,([P "responses",P "200"]
     ,("X",String "responses-200-X-value"))
    ,([P "tags",P "items"]
     ,("X",String "tags-items-X-value-2"))]

ts4 :: [Test]
ts4 = U.t "ts4"
    (findInJson
     "X"
     (Object
      (fromList
       [("default" ,Object (fromList [("X",String "responses-default-X-value")]))
       ,("200"     ,Object (fromList [("schema",Object (fromList [("items",Object (fromList [("X",String "responses-200-schema-items-X-value")]))
                                                                 ,("type",String "array")]))
                                     ,("X",String "responses-200-X-value")]))])))
    [([P "default"]                  ,("X",String "responses-default-X-value"))
    ,([P "200",P "schema",P "items"] ,("X",String "responses-200-schema-items-X-value"))
    ,([P "200"]                      ,("X",String "responses-200-X-value"))]
