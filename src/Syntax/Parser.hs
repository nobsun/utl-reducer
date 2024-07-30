-- # Syntax.Parser
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, LexicalNegation, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType #-}
{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors, DuplicateRecordFields, OverloadedRecordDot #-}
module Syntax.Parser
    (
    ) where

import Control.Arrow
import Data.Char

import Syntax.Term

data Token
    = Ide String
    | Lam
    | Dot
    | LParen
    | RParen
    | Semi
    deriving (Eq, Show)

clex :: String -> [Token]
clex = \ case
    []      -> []
    '\\':cs -> Lam : clex cs
    '.' :cs -> Dot : clex cs
    '(' :cs -> LParen : clex cs
    ')' :cs -> RParen : clex cs
    ';' :cs -> Semi : clex cs
    ccs@(c:cs)
        | isSpace  c -> clex cs
        | otherwise  -> case span isIdChar ccs of
            ("",_)       -> error "invalid char"
            (vs,cs')     -> Ide vs : clex cs'

isIdChar :: Char -> Bool
isIdChar c = isAscii c && (isAlpha c || c == '_')

newtype Parser a = 
    Parser { parser :: [Token] -> [(a, [Token])] }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ fmap (first f) . p.parser

pSat :: (Token -> Bool) -> Parser Token
pSat p = Parser $ \ case
    []          -> []
    tok:toks
        | p tok -> [(tok, toks)]
    _           -> []

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ (++) <$> p.parser <*> q.parser

infixr 6 +++

(<++) :: Parser a -> Parser a -> Parser a
p <++ q = Parser $ (<+) <$> p.parser <*> q.parser

(<+) :: [a] -> [a] -> [a]
[] <+ y = y
x  <+ _ = x

infixr 5 <+
