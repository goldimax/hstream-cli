{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Type where


import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Data.Data (Typeable)
import Data.Swagger
import Data.UUID

-- | Response
-- OK
-- Failed
data Resp
    = OK Text     
    deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)



-- | Require SQL
-- required sql 
data ReqSql 
    = ReqSql {
      -- | required version
      rver :: Int
      -- | sql value
    , rsql :: Text 
    }
    deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)

data DatabaseInfo 
    = DatabaseInfo {
        dbid :: DatabaseId
      , dbtables :: [TableId]
      , dbstreams :: [StreamId]
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)


data Database 
    = Database {
      dbname :: Text
    , dbc :: Text
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)

type DatabaseId =  UUID

data TableInfo
    = TableInfo {
        tsfbiname :: Text 
      , tbid :: UUID
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)

type TableId =  UUID

data Table 
    = Table {
        ctname :: Text
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)

data StreamInfo
    = StreamInfo {
        strinfo :: Text
      , strid :: StreamId
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)

data StreamSql 
    = StreamSql {
        strname :: Text
      , streamSql :: ReqSql
    } deriving (Show, Eq, Generic, Typeable, FromJSON, ToJSON)

type StreamId =  UUID



instance ToSchema Resp
instance ToSchema ReqSql
instance ToSchema Database
instance ToSchema DatabaseInfo
instance ToSchema Table
instance ToSchema TableInfo
instance ToSchema StreamSql
instance ToSchema StreamInfo




