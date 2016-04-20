{-# LANGUAGE OverloadedStrings #-}

module Data.Json.Visualization where

import qualified Data.Aeson           as A (Value (String))
import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz        as GV
import           Data.Json.Validation
import qualified Data.List            as L
import qualified Data.Map.Strict      as M
import qualified Data.Maybe           as MB
import qualified Data.Text            as T
import           Prelude              as P

toMap :: PathFieldValue -> M.Map T.Text [T.Text]
toMap = foldr go M.empty
  where
    go :: ([T.Text], (T.Text, A.Value)) -> M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
    go (path,(_,A.String value)) acc =
        M.insertWith (++)
                     (T.intercalate "/" (P.take 2 path))  -- only use first part of path
                     [T.drop 2 value]                     -- do not include "#/"
                     acc
    go  _ _ = error "NO"

mkNodeMap :: M.Map T.Text [T.Text] -> M.Map T.Text (G.LNode T.Text)
mkNodeMap m = snd $ foldr go (0::Int, M.empty) (L.nub $ M.keys m ++ P.concat (M.elems m))
  where
    go x (n,acc) = (n+1, M.insert x (n,x) acc)

mapToGraph :: M.Map T.Text [T.Text] -> G.Gr T.Text ()
mapToGraph m =
    let nodeMap     = mkNodeMap m
        edges       = M.foldrWithKey go [] m
        go k vs acc = foldr (go' k) acc vs
        go' k v acc = ( fst $ MB.fromJust $ M.lookup k nodeMap
                      , fst $ MB.fromJust $ M.lookup v nodeMap
                      , ()) : acc
    in G.mkGraph (M.elems nodeMap) edges

toDot :: PathFieldValue -> GV.DotGraph G.Node
toDot = GV.graphToDot GV.nonClusteredParams . mapToGraph . toMap

{-
import Data.Text
:set -XOverloadedStrings
(Just v) <- readJson "test/refs-simple-invalid.json"
let pfvs = findInJson "$ref" v
let m = toMap pfvs
mkNodeMap m
let dg = (mapToGraph . toMap) pfvs
let td = Data.Json.Visualization.toDot pfvs
runGraphviz td Png "/tmp/xx"
-}
