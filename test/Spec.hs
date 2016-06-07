{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Object, String))
import           Data.HashMap.Strict as HM (fromList)
import           Data.Json.Util
import           Data.Text           as T (Text)
import           System.Directory    (doesFileExist)
import           System.IO.Unsafe    (unsafePerformIO)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import           Test.HUnit.Util     as U (t)

main :: IO Counts
main = runTestTT $ TestList $ ts1 ++ ts2 ++ ts3 ++ ts4

iRead :: FilePath -> IO FilePath
iRead filename = do
    e <- doesFileExist filename
    return $ if e then filename else "test/" ++ filename

readFind :: Text -> FilePath -> IO PathsPropertyNameValue
readFind goal filename = iRead filename >>= (readJson >=> (\(Just s) -> return $ findInJson goal s))

ts1 :: [Test]
ts1 = U.t "ts1"
    (unsafePerformIO (readFind "$ref" "refs-simple-invalid.json"))
    [(["definitions","Parent"]                                  ,("$ref",String "#/definitions/DoesNotExist"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

ts2 :: [Test]
ts2 = U.t "ts2"
    (unsafePerformIO (readFind "$ref" "refs-indirect-circular-ancestor-invalid.json"))
    [(["definitions","Parent","allOf"]                          ,("$ref",String "#/definitions/Circular2"))
    ,(["definitions","Circular1","allOf"]                       ,("$ref",String "#/definitions/Parent"))
    ,(["definitions","Circular2","allOf"]                       ,("$ref",String "#/definitions/Circular1"))
    ,(["definitions","Pet","properties","category"]             ,("$ref",String "#/definitions/Category"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

ts3 :: [Test]
ts3 = U.t "ts3"
    (unsafePerformIO (readFind "X" "x.json"))
    [(["Tag"]                              ,("X",Object (fromList [("Tag-X-value",Object (fromList [("X",String "Tag-X-value-X-value")]))])))
    ,(["responses","default"]              ,("X",String "responses-default-X-value"))
    ,(["responses","200","schema","items"] ,("X",String "responses-200-schema-items-X-value"))
    ,(["responses","200"]                  ,("X",String "responses-200-X-value"))
    ,(["tags","items"]                     ,("X",String "tags-items-X-value-2"))]

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
    [(["default"]               ,("X",String "responses-default-X-value"))
    ,(["200","schema","items"]  ,("X",String "responses-200-schema-items-X-value"))
    ,(["200"]                   ,("X",String "responses-200-X-value"))]
