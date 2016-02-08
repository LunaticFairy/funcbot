{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Yaml.Parser
import qualified Data.Text as T
import System.IO

data ConnectionInfo = ConnectionInfo 
    { address :: !T.Text
    , port :: !Int
    , nickname :: !T.Text  
    , username :: !T.Text
    , realname :: !T.Text
    , nickserv :: !T.Text
    , channels :: ![T.Text]
    } deriving (Show)

instance FromYaml ConnectionInfo where
    fromYaml = withMapping "connection" $ \o -> ConnectionInfo 
        <$> o .: "address"
        <*> o .: "port"
        <*> o .: "nickname"
        <*> o .: "username"
        <*> o .: "realname"
        <*> o .: "nickserv"
        <*> o .: "channels"

readConnectionInfo = do
    f <- readYamlFile "./config.yaml"
    return (f :: ConnectionInfo)
