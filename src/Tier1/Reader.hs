module Tier1.Reader (cd, su) where

import Control.Monad.Reader
import Tier0.Reader (Environment (..), EnvironmentM)

cd :: String -> EnvironmentM a -> EnvironmentM a
cd dir env = local (\arg_env -> arg_env {currentDir = (currentDir arg_env) ++ "/" ++ dir}) env


su :: EnvironmentM a -> EnvironmentM a
su env = local (\arg_env -> arg_env {isSuperUser = True}) env 
