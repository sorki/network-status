{-# LANGUAGE OverloadedStrings #-}

module StatusSpec where

import SpecHelper

import System.Process
import System.Posix.User
import System.Linux.Namespaces

spec :: Spec
spec = do
  it "wurkz" $ do
    -- find the uid, we must do that before unshare
    uid <- getEffectiveUserID
    unshare [User, Network]
    -- map current user to user 0 (i.e. root) inside the namespace
    writeUserMappings Nothing [UserMapping 0 uid 1]

    -- XXX: acually do something here

    --watch
    return ()

main :: IO ()
main = hspec spec
