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

data GraphData = GraphData
    { graph      :: [Value]
    , links      :: [Link]
    , multigraph :: Bool
    , nodes      :: [Node]
    , directed   :: Bool
    } deriving (Show, Generic)

data Link = Link
    { source   :: Int
    , vpn      :: Bool
    , target   :: Int
    , bidirect :: Bool
    , tq       :: Double
    } deriving (Show, Generic)

data Node = Node
    { id      :: String
    , node_id :: Maybe Value
    } deriving (Show, Generic)

instance FromJSON GraphJson
instance FromJSON GraphData
instance FromJSON Link
instance FromJSON Node
