{-# LANGUAGE OverloadedStrings #-}
module TransitionSpec
  ( spec
  ) where

import Data.String
import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString as B
import Test.Main
import Test.Hspec
import Text.Show.Unicode
import Transition

newtype UString a = UString a deriving Eq

ustring :: B.ByteString -> UString String
ustring = UString . U.decode . B.unpack

instance IsString a => IsString (UString a) where
  fromString = UString . fromString
  
instance Show a => Show (UString a) where
  show (UString s) = ushow s

spec :: Spec
spec = describe "responser"
  $ it "任意の入力文字列を\"なんか関数\"に変換" 
  $ map UString (responser undefined ["Hi!", "元気?"]) `shouldBe` ["なんか関数", "なんか関数"]

-- spec :: Spec
-- spec = describe "someFunc" $ do
--   { it "「なんか関数」を標準出力に印字する." $ do
--     { result <- captureProcessResult Lib.someFunc
--     ; prExitCode result `shouldBe` ExitSuccess
--     ; prStderr result `shouldSatisfy` B.null
--     ; ustring (prStdout result) `shouldBe` "なんか関数\n"
--     }
--   }
