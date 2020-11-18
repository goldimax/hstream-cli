{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler where

import Api
import Type
import Servant.Server
import Servant
import Servant.Swagger
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Aeson
import Control.Monad.IO.Class
import Data.UUID
import Data.UUID.V1
import System.Random
import qualified Data.Text.IO as TIO

server1API :: Proxy ServerAPI1
server1API = Proxy

server1 :: Server ServerAPI1
server1  = handle_database :<|> handle_table :<|> handle_stream

handle_database = handle_show_databases :<|> handle_create_databae :<|> handle_use_database

handle_table = handle_show_tables :<|> handle_create_table :<|> handle_query_table :<|> handle_delete_table

handle_stream = handle_show_streams :<|> handle_create_stream :<|> handle_query_stream :<|> handle_delete_stream

handle_show_databases   = sequence $ replicate 3 $ (DatabaseInfo <$> getUUID <*> gs <*> gs)
handle_create_databae _ = DatabaseInfo <$> getUUID <*> getUUIDs 0 <*> getUUIDs 0
handle_use_database   _ = return $ OK "succuess"

handle_show_tables      = sequence $ replicate 3 $ TableInfo <$> pure "table name" <*> getUUID
handle_create_table   t = TableInfo <$> pure (ctname t) <*> getUUID
handle_query_table    t = TableInfo <$> pure "random table" <*> pure t
handle_delete_table   _ = return $ OK "delete table success"

handle_show_streams     = sequence $ replicate 3 $ StreamInfo <$> pure "stream name" <*> getUUID
handle_create_stream  t = StreamInfo <$> pure (strname t) <*> getUUID
handle_query_stream   t = StreamInfo <$> pure "random stream value" <*> pure t
handle_delete_stream  _ = return $ OK "delete stream success"

app1 :: Application
app1 = serve server1API server1

getUUID :: Handler UUID 
getUUID = (liftIO nextUUID ) >>= \case 
    Nothing -> getUUID
    Just v  -> return v

gs = getUUIDs 4 

getUUIDs :: Int -> Handler [UUID]
getUUIDs v = do 
    n <- liftIO $ randomRIO (0, v)
    sequence $ replicate n getUUID

sw = BSL8.putStrLn $ encode $ toSwagger (Proxy :: Proxy ServerAPI1)

sl :: IO ()
sl = do 
    TIO.writeFile "resutl.txt" (layout server1API) 








