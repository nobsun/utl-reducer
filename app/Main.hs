{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, LexicalNegation, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType #-}
{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors, DuplicateRecordFields, OverloadedRecordDot #-}
module Main where

import Control.Concurrent.Chan ( newChan )
-- import Control.Exception
import Data.Char ( ord, toUpper )
import Data.List.Extra ( list )
import Interaction.Interact
    ( defaultInputConfig,
      genericInteract,
      inputLines,
      inputLines',
      outputLines,
      outputLines',
      outputLinesWithPrompt,
      outputLinesWithPrompt',
      Prompt )
import Interaction.Interacts ( interacts, timeoutMsg, tenSecs )
import System.Environment ( getArgs )

main :: IO ()
main = (interactions !!) . (`mod` len0) 
   =<< list (return 0) (((return . read) .) . const)
   =<< getArgs 

interactions :: [IO ()]
interactions = [foo, foo', bar, bar', baz, baz', qux, qux'
               ,soko]

len0 :: Int
len0 = length interactions

toUpperCase :: [String] -> [String]
toUpperCase = map upcase

upcase :: String -> String
upcase = show . map (ord . toUpper)

toUpperCaseWithPrompt :: Prompt -> [String] -> [String]
toUpperCaseWithPrompt prompt = (prompt :) . map ((++ '\n':prompt) . upcase) 

foo :: IO ()
foo = genericInteract inputLines 
                      outputLines 
                      toUpperCase

foo' :: IO ()
foo' = genericInteract inputLines 
                       outputLinesWithPrompt
                       (toUpperCaseWithPrompt "? ")

bar :: IO ()
bar = genericInteract (inputLines' defaultInputConfig) 
                      outputLines
                      toUpperCase

bar' :: IO ()
bar' = genericInteract (inputLines' defaultInputConfig)
                       outputLinesWithPrompt
                       (toUpperCaseWithPrompt "? ")

baz :: IO ()
baz = do
    { chan <- newChan
    ; genericInteract inputLines
                      (outputLines' chan)
                      toUpperCase
    }
baz' :: IO ()
baz' = do
    { chan <- newChan
    ; genericInteract inputLines
                      (outputLinesWithPrompt' chan) 
                      (toUpperCaseWithPrompt "? ") 
    }

qux :: IO ()
qux = do
    { chan <- newChan
    ; genericInteract (inputLines' defaultInputConfig)
                      (outputLines' chan)
                      toUpperCase
    }

qux' :: IO ()
qux' = do
    { chan <- newChan
    ; genericInteract (inputLines' defaultInputConfig)
                      (outputLinesWithPrompt' chan)
                      (toUpperCaseWithPrompt "? ")
    }

soko :: IO ()
soko = do
    { chan <- newChan
    ; let { ?msg = timeoutMsg; ?tm = tenSecs }
    ; interacts (map (map toUpper)) 
    }
