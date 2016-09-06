{-# LANGUAGE OverloadedStrings #-}
module BookServer.Server (runServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Servant

import BookServer.API as API
import qualified BookServer.Database as Database
import BookServer.Types

-- type Handler a = EitherT ServantErr IO a

server :: Server BookAPI
server = listBooks

listBooks :: Maybe Text -> Handler Bookshelf
listBooks q = do
  liftIO $ Database.listBooks q

middleware = logStdout . simpleCors
app :: Application
app = middleware (serve API.bookAPI server)

runServer :: Port -> IO ()
runServer port = run port app
