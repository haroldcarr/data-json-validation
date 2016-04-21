{-# LANGUAGE OverloadedStrings #-}

module Data.Json.Visualization where

import qualified Data.Aeson                        as A (Value (String))
import qualified Data.Graph.Inductive              as G
import qualified Data.GraphViz                     as GV
import qualified Data.GraphViz.Attributes.Complete as GV
import           Data.Json.Validation
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Prelude                           as P

pToG :: PathFieldValue -> G.Gr T.Text ()
pToG  = fst . uncurry G.mkMapGraph . foldr go ([],[])
  where
    go (path, (_,A.String value)) (ns,es) =
        let n1 = T.intercalate "/" (P.take 2 path)  -- only use first part of path
            n2 = T.drop 2 value                     -- do not include "#/"
        in (n1:n2:ns,(n1,n2,()):es)
    go                         _     acc  = acc     -- this should not happen

gToD :: G.Gr T.Text () -> GV.DotGraph G.Node
gToD  = GV.graphToDot params
  where
    params = GV.nonClusteredParams
        { GV.fmtNode = \(_, l) -> [GV.Label $ GV.StrLabel (TL.fromStrict l)]
        }

toDot :: PathFieldValue -> GV.DotGraph G.Node
toDot  = gToD . pToG
