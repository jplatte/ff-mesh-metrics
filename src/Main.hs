{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings #-}

module Main where

import Prelude hiding (interact)
import Control.Arrow ((>>>), first, second)
import Control.Monad.State
import Data.Aeson (Value(), (.=), encode, decode, object)
import Data.ByteString.Lazy (interact)
import Data.Char (isHexDigit)
import Data.List ((\\), elemIndex, genericLength, nub)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Safe (fromJustNote, maximumMay)

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
genMetrics' nodesAndGWs allLinks = object
    [ "link_count"   .= length wifiLinks
    , "mesh_rate"    .= meshRate (length nodes) (length subnets)
    , "meshsize_max" .= maximumMay subnetSizes
    , "meshsize_avg" .= (genericLength nodes / genericLength subnets :: Double)
    ]
    where nodes       = filter (not . isGW) nodesAndGWs
          wifiLinks   = filter (not . vpn) allLinks
          subnets     = meshNets nodesAndGWs nodes wifiLinks
          subnetSizes = length <$> subnets

isGW :: Node -> Bool
isGW (Node _ Nothing)    = False
isGW (Node _ (Just str)) = any (not . isHexDigit) str

meshNets :: [Node] -> [Node] -> [Link] -> [[Node]]
meshNets nodesAndGWs nodes links = evalState (meshNets' nodesAndGWs) (nodes, links)

meshNets' :: [Node] -> State ([Node], [Link]) [[Node]]
meshNets' nodesAndGWs = do
    node   <- gets (fst >>> head)
    subNet <- getSubNet node

    gets (fst >>> null) >>= \case
        False -> (subNet :) <$> meshNets' nodesAndGWs
        True  -> return [subNet]

    where
        hasNode :: Node -> Link -> Bool
        hasNode node (Link source _ target _ _) =
            source == nodesAndGWsIdx node || target == nodesAndGWsIdx node

        nodesAndGWsIdx :: Node -> Int
        nodesAndGWsIdx n = fromJust $ elemIndex n nodesAndGWs

        getSubNet :: Node -> State ([Node], [Link]) [Node]
        getSubNet node = do
            modify $ first (\\ [node])
            links <- gets (snd >>> filter (hasNode node))

            if not (null links) then do
                modify $ second (\\ links)

                let nodeIdx = nodesAndGWsIdx node
                    peers = fmap
                        (\(Link source _ target _ _) ->
                            if source == nodeIdx
                            then nodesAndGWs !! target
                            else nodesAndGWs !! source)
                        links

                nub . (node :) . concat <$> mapM getSubNet peers
            else
                return [node]

meshRate :: (Integral a)
         => a  -- nodes
         -> a  -- subnets
         -> Maybe Double
meshRate nodes subnets
    | nodes < 2 || subnets < 1 || nodes < subnets = Nothing
    | otherwise = Just ((nodes' - subnets') / (nodes' - 1))
    where nodes'   = fromIntegral nodes
          subnets' = fromIntegral subnets
