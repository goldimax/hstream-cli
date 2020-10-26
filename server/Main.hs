{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Handler
import Network.Wai.Handler.Warp

main :: IO()
main = do 
    print "server start"
    run 8081 app1

