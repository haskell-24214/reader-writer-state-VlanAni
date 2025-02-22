module Tier0.Reader (Environment (..), EnvironmentM, formatUserName, formatHost, formatCurrentDir, formatPrompt) where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

formatUserName :: EnvironmentM String
formatUserName = do
  isSuper <- asks isSuperUser
  name <- asks username 
  if isSuper
    then return "root"
    else return name
  
formatHost :: EnvironmentM String
formatHost = do 
  user_host <- asks host 
  return user_host

formatCurrentDir :: EnvironmentM String
formatCurrentDir = do
  user_CurrentDir <- asks currentDir
  return user_CurrentDir

formatPrompt :: EnvironmentM String
formatPrompt = do 
  name <- formatUserName
  hostname <- formatHost
  currDir <- formatCurrentDir 
  return $ name ++ "@" ++ hostname ++ ":" ++ currDir ++ "$"
