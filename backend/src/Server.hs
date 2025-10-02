{-# LANGUAGE OverloadedStrings #-}

module Server
  ( startServer
  ) where

import Web.Scotty (scotty, middleware, Options(..))
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods)
import UserRepository (createInMemoryUserRepository)
import API (apiRoutes, AppEnv(..))
import Control.Monad.IO.Class (liftIO)

-- | Start the web server
startServer :: IO ()
startServer = do
  -- Initialize dependencies
  userRepo <- createInMemoryUserRepository
  let appEnv = AppEnv { userRepository = userRepo }

  -- Configure CORS
  let corsPolicy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type", "Authorization"]
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        }

  -- Start server
  scotty 3000 $ do
    -- Add CORS middleware
    middleware $ cors (const $ Just corsPolicy)

    -- Add API routes
    apiRoutes appEnv
