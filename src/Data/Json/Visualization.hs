{-# LANGUAGE OverloadedStrings #-}

module Data.Json.Visualization where

import qualified Data.Aeson           as A (Value (String))
import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz        as GV
import           Data.Json.Validation
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           Prelude              as P

toMap :: PathFieldValue -> M.Map T.Text [T.Text]
toMap = foldr go M.empty
  where
    go :: ([T.Text], (T.Text, A.Value)) -> M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
    go (path,(_,A.String value)) =
        M.insertWith (++)
                     (T.intercalate "/" (P.take 2 path))
                     [T.drop 2 value]

-- mapToGraph :: (G.Graph gr) => M.Map T.Text [T.Text] -> gr T.Text T.Text
mapToGraph :: M.Map T.Text [T.Text] -> G.Gr T.Text T.Text
mapToGraph m = go (M.toList m) 0 [] []
  where
    go          []  _ nodes0 edges0 = G.mkGraph nodes0 edges0
    go ((k,vs):kvs) n nodes0 edges0 = let (n',moreNodes) = foldr go' (n,[]) vs
                                      in go kvs n' (nodes0 ++ moreNodes) edges0
    go' v (n,acc)                   = (n+1, (n, v) : acc)

toDot :: PathFieldValue -> GV.DotGraph G.Node
toDot = GV.graphToDot GV.nonClusteredParams . mapToGraph . toMap

{-
import Data.Text
:set -XOverloadedStrings
(Just v) <- readJson "test/refs-simple-invalid.json"
let pfvs = findInJson "$ref" v
toMap pfvs
toDot pfvs
-}
