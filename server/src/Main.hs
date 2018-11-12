{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import qualified Data.Char as C
import qualified Data.Text as T
import Data.HashMap.Strict (fromList)
import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty
import Network.Wai.Middleware.Cors

instance ToJSON Guide
instance FromJSON Guide

data Guide = Guide 
  { id :: Int
  , title :: String
  } deriving (Show, Generic)


guides =
  [ Guide 1 "Introduction"
    , Guide 2 "Preparing your environment"
    , Guide 3 "Conclusion"
  ]

urls = fromList
  [ ("Say hello", String "/hello")
  , ("List of guides", String "/guides")
  ]

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
    middleware simpleCors
    get "/" $ do
      json $ Object urls


    get "/hello" $ do
      text "hello world!"

    get "/guides" $ do
      json guides
