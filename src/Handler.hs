{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handler where

import Api (ServerAPI1)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text.IO as TIO
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Servant
  ( Application,
    Handler,
    Proxy (..),
    Server,
    layout,
    serve,
    type (:<|>) ((:<|>)),
  )
import Servant.Swagger (HasSwagger (toSwagger))
import System.Random (Random (randomRIO))
import Type
  ( DatabaseInfo (DatabaseInfo),
    Resp (OK),
    StreamId,
    StreamInfo (StreamInfo),
    StreamSql (strname),
    Table (ctname),
    TableInfo (TableInfo),
  )

server1API :: Proxy ServerAPI1
server1API = Proxy

server1 :: Server ServerAPI1
server1 = handleDatabase :<|> handleTable :<|> handleStream

handleDatabase :: Handler [DatabaseInfo] :<|> ((p1 -> Handler DatabaseInfo) :<|> (p2 -> Handler Resp))
handleDatabase = handleShowDatabases :<|> handleCreateDatabae :<|> handleUseDatabase

handleTable :: Handler [TableInfo] :<|> ((Table -> Handler TableInfo) :<|> ((UUID -> Handler TableInfo) :<|> (p -> Handler Resp)))
handleTable = handleShowTables :<|> handleCreateTable :<|> handleQueryTable :<|> handleDeleteTable

handleStream :: Handler [StreamInfo] :<|> ((StreamSql -> Handler StreamInfo) :<|> ((StreamId -> Handler StreamInfo) :<|> (p -> Handler Resp)))
handleStream = handleShowStreams :<|> handleCreateStream :<|> handleQueryStream :<|> handleDeleteStream

handleShowDatabases :: Handler [DatabaseInfo]
handleShowDatabases = replicateM 3 (DatabaseInfo <$> getUUID <*> gs <*> gs)

handleCreateDatabae :: p -> Handler DatabaseInfo
handleCreateDatabae _ = DatabaseInfo <$> getUUID <*> getUUIDs 0 <*> getUUIDs 0

handleUseDatabase :: Applicative m => p -> m Resp
handleUseDatabase _ = pure $ OK "succuess"

handleShowTables :: Handler [TableInfo]
handleShowTables = replicateM 3 $ TableInfo "table name" <$> getUUID

handleCreateTable :: Table -> Handler TableInfo
handleCreateTable t = TableInfo (ctname t) <$> getUUID

handleQueryTable :: Applicative f => UUID -> f TableInfo
handleQueryTable t = pure $ TableInfo "random table" t

handleDeleteTable :: Applicative m => p -> m Resp
handleDeleteTable _ = pure $ OK "delete table success"

handleShowStreams :: Handler [StreamInfo]
handleShowStreams = replicateM 3 $ StreamInfo "stream name" <$> getUUID

handleCreateStream :: StreamSql -> Handler StreamInfo
handleCreateStream t = StreamInfo (strname t) <$> getUUID

handleQueryStream :: Applicative f => StreamId -> f StreamInfo
handleQueryStream t = pure $ StreamInfo "random stream value" t

handleDeleteStream :: Monad m => p -> m Resp
handleDeleteStream _ = return $ OK "delete stream success"

app1 :: Application
app1 = serve server1API server1

getUUID :: Handler UUID
getUUID =
  liftIO nextUUID >>= \case
    Nothing -> getUUID
    Just v -> return v

gs :: Handler [UUID]
gs = getUUIDs 4

getUUIDs :: Int -> Handler [UUID]
getUUIDs v = do
  n <- liftIO $ randomRIO (0, v)
  replicateM n getUUID

sw :: IO ()
sw = BSL8.putStrLn $ encode $ toSwagger (Proxy :: Proxy ServerAPI1)

sl :: IO ()
sl = do
  TIO.writeFile "resutl.txt" (layout server1API)
