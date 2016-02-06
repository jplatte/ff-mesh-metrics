{-# LANGUAGE DeriveGeneric #-}

module GraphJson
( GraphJson(..)
, GraphData(..)
, Link(..)
, Node(..)
) where

import Data.Aeson
import GHC.Generics

{-# ANN module "HLint: ignore Use camelCase" #-}

data GraphJson = GraphJson
    { batadv  :: GraphData
    , version :: Int
    } deriving (Show, Generic)

instance FromJSON GraphJson

data GraphData = GraphData
    { graph      :: [Value]
    , links      :: [Link]
    , multigraph :: Bool
    , nodes      :: [Node]
    , directed   :: Bool
    } deriving (Show, Generic)

instance FromJSON GraphData

data Link = Link
    { source   :: Int
    , vpn      :: Bool
    , target   :: Int
    , bidirect :: Bool
    , tq       :: Double
    } deriving (Show, Generic)

instance Eq Link where
    (Link s1 _ t1 _ _) == (Link s2 _ t2 _ _) =  s1 == s2 && t1 == t2
instance FromJSON Link

data Node = Node
    { id      :: String
    , node_id :: Maybe String
    } deriving (Show, Generic)

instance Eq Node where
    (Node id1 _) == (Node id2 _) = id1 == id2
instance FromJSON Node
