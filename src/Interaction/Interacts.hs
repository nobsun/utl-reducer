{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, LexicalNegation, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType #-}
{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors, DuplicateRecordFields, OverloadedRecordDot #-}
module Interaction.Interacts
    ( interacts
    , getLines
    , getLine'
    , timeoutMsg
    , tenSecs
    ) where

import Control.Concurrent
    ( Chan, getChanContents, newChan, writeChan, forkIO )
import Control.Exception
    ( handle, BlockedIndefinitelyOnMVar, IOException )
import System.Timeout ( timeout )

interacts :: (?msg :: String, ?tm :: Int)
          => ([String] -> [String]) -> IO ()
interacts f = do
    { ch <- newChan
    ; _  <- forkIO (getLines ch)
    ; handle (const done :: BlockedIndefinitelyOnMVar -> IO ())
    $ putStr . unlines . f =<< getChanContents ch
    }

getLines :: (?msg :: String, ?tm :: Int)
         => Chan String -> IO ()
getLines chan
    = handle (const done :: IOException -> IO ())
    $ getLine' >>= writeChan chan >> getLines chan

getLine' :: (?msg :: String, ?tm :: Int) => IO String
getLine' = maybe (return ?msg) return =<< timeout ?tm getLine

done :: IO ()
done = return ()

timeoutMsg :: String
timeoutMsg = "Time out!"

tenSecs :: Int
tenSecs = 10 * 10 ^ (6 :: Int)