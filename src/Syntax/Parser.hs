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

-- Term Parser

type TermParser = Parser Term

pTerm :: TermParser
pTerm = undefined


-- Parser コンビネータ

newtype Parser a = 
    Parser { parser :: [Token] -> [(a, [Token])] }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ fmap (first f) . p.parser

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \ toks -> [(x, toks)]
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p <*> q = Parser $ \ toks ->
                [ (s t, ss) | (s, rs) <- p.parser toks, (t, ss) <- q.parser rs ]

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser $ \ toks ->
                [ (t,ss) | (s, rs) <- p.parser toks, (t, ss) <- (f s).parser rs ]

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

pMany1 :: Parser a -> Parser [a]
pMany1 = undefined

pMany :: Parser a -> Parser [a]
pMany p = pMany1 p <++ pure []