{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings #-}

module Main where

import Prelude hiding (interact)
import Control.Arrow ((>>>))
import Data.Aeson (Value, (.=), encode, decode, object)
import Data.ByteString.Lazy (interact)
import Data.Monoid ((<>))
import Safe (fromJustNote)

import GraphJson

main :: IO ()
main = interact $ decode
    >>> fromJustNote "Parsing failed!"
    >>> genMetrics
    >>> \case Left errorMsg -> error errorMsg
              Right metrics -> metrics
    >>> encode
    >>> (<> "\n")

genMetrics :: GraphJson -> Either String Value
genMetrics (GraphJson (GraphData _ links multigraph nodes directed) version) =
    if | version /= 1 -> Left $ "Expected version 1, got " ++ show version
       | multigraph   -> Left   "Expected multigraph = false"
       | directed     -> Left   "Expected directed = false"
       | otherwise    -> Right $ genMetrics' nodes links

genMetrics' :: [Node] -> [Link] -> Value
genMetrics' nodes links = object
    [ "link_count"   .= length links
    --, "mesh_rate"    .= undefined
    --, "meshsize_max" .= undefined
    --, "meshsize_avg" .= undefined
    ]
