{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import System.Console.Haskeline
import Control.Monad.IO.Class
import Network.HTTP.Simple hiding (Proxy)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import Data.Aeson
import GHC.Generics (Generic)
import Data.Data (Typeable)
import Control.Exception (try)
import Type 
import Options.Applicative
import Data.Proxy
import Text.Pretty.Simple
import Data.UUID
import Data.Maybe
import System.Console.Haskeline.Completion

data Config 
    = Config {
      curl :: String 
    , cport :: Int
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)


parseConfig :: Parser Config 
parseConfig = Config 
    <$> strOption (long "baseUrl" <> metavar "string" <> short 'b' <> help "base url valur")
    <*> option auto (long "port" <> value 8081 <> short 'p' <> help "port value" <> metavar "INT")

testConfig :: Config
testConfig = Config "http://localhost" 8081

def = setComplete compE defaultSettings 

compE = completeWord Nothing [] compword

compword :: Monad m => String -> m [Completion]
compword  "s" = return [simpleCompletion "show"]
compword  "sh" = return [simpleCompletion "show"]
compword  "sho" = return [simpleCompletion "show"]
compword  "show " = return $ map simpleCompletion ["show tables", "show databases", "show streams"]
compword  "t" = return [simpleCompletion "table"]
compword  "" = return $ map simpleCompletion ["show", "query", "delete", "create", "use"]
compword _ = return []
-- compword  "show" = return $ map simpleCompletion ["database", "stream", "table"]

main :: IO ()
main = do 
    putStrLn "Start Hstream-CLI!"
    cf <- execParser $ info (parseConfig <**> helper) (fullDesc <> progDesc "start hstream-cli" )
    runInputT def $ loop cf
   where
       loop :: Config -> InputT IO ()
       loop c@Config{..} = do
           minput <- getInputLine "> "
           case minput of
               Nothing -> return ()
               Just ":q" -> liftIO  (putStrLn "Finish!") >> return ()
               Just xs -> do
                 case words xs of 
                   "show" : "databases" : _ -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/show/databases") >>= handleReq (Proxy :: Proxy [DatabaseInfo])

                   "create" : "database" : name : content -> do 
                        re <- liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/create/database")
                        liftIO $ handleReq (Proxy :: Proxy DatabaseInfo) 
                            $ setRequestBodyJSON (Database (pack name) (pack $ unwords content)) 
                            $ setRequestMethod "POST" re
                   "use" : "database" : dbid -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/use/" ++ unwords dbid) >>= handleReq (Proxy :: Proxy Resp)

                        ---------------------------------------------------------------------------------------------------------------

                   "show" : "tables" : _ -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/show/tables") >>= handleReq (Proxy :: Proxy [TableInfo])
                   "create" : "table" : name  -> do 
                        re <- liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/create/table")
                        liftIO $ handleReq (Proxy :: Proxy TableInfo) 
                            $ setRequestBodyJSON (Table (pack $ unwords name)) 
                            $ setRequestMethod "POST" re
                   "query" : "table" : tbid -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/query/table/" ++ unwords tbid) >>= handleReq (Proxy :: Proxy TableInfo)
                   "delete" : "table" : dbid -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/delete/table/" ++ unwords dbid) >>= handleReq (Proxy :: Proxy Resp)

                        ---------------------------------------------------------------------------------------------------------------

                   "show" : "streams" : _ -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/show/streams") >>= handleReq (Proxy :: Proxy [StreamInfo])
                   "create" : "stream" : name : sql  -> do 
                        re <- liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/create/stream")
                        liftIO $ handleReq (Proxy :: Proxy StreamInfo) 
                            $ setRequestBodyJSON (StreamSql (pack $ name) (ReqSql 1 (pack $ unwords sql))) 
                            $ setRequestMethod "POST" re
                   "query" : "stream" : tbid -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/query/stream/" ++ unwords tbid) >>= handleReq (Proxy :: Proxy StreamInfo)
                   "delete" : "stream" : dbid -> 
                        liftIO $ parseRequest (curl ++ ":" ++ show cport ++ "/delete/stream/" ++ unwords dbid) >>= handleReq (Proxy :: Proxy Resp)
                   [] -> return ()
                   _  -> liftIO $ putStrLn "invalid input"

                 loop c

handleReq :: forall a. (Show a, FromJSON a) => Proxy a -> Request -> IO ()
handleReq Proxy req = do 
    (v :: Either SomeException (Response ByteString)) <- try $ httpBS req 
    case v of
        Left e -> print e 
        Right a -> do 
            case getResponseBody a of 
                "" -> putStrLn "invalid command"
                ot -> case eitherDecode' (BL.fromStrict ot) of 
                          Left e -> print e
                          Right (rsp :: a) -> pPrint rsp





