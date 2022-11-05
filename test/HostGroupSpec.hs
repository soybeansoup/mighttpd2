{-# LANGUAGE OverloadedStrings #-}

module HostGroupSpec where

import Test.Hspec

import Program.Mighty

spec :: Spec
spec = do
    describe "parseRoute" $ do
        it "parses hostgroup.route correctly" $ do
            res <- parseRoute "conf/hostgroup.route" "localhost" 80
            res `shouldBe` ans

ans :: [Block]
ans = [Block ["localhost"] [RouteRevProxy "/" "/" [("localhost", 55000),("localhost", 55001)]]]
