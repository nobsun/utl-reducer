-- # Term
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, LexicalNegation, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType #-}
{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors, DuplicateRecordFields, OverloadedRecordDot #-}
module Syntax.Term where

import Control.Comonad.Trans.Cofree qualified as F
import Control.Comonad.Cofree
import Data.Functor.Foldable

type Var = String

data Term
    = Var Var
    | Abs Var Term
    | App Term Term
    deriving (Eq, Show)

data TermF r
    = VarF Var
    | AbsF Var r
    | AppF r r

instance Functor TermF where
    fmap f = \ case
        VarF x   -> VarF x
        AbsF x t -> AbsF x (f t)
        AppF t u -> AppF (f t) (f u)

type instance Base Term = TermF

instance Recursive Term where
    project :: Term -> Base Term Term
    project = \ case
        Var x   -> VarF x
        Abs x t -> AbsF x t
        App t u -> AppF t u

instance Corecursive Term where
    embed :: Base Term Term ->  Term
    embed = \ case
        VarF x   -> Var x
        AbsF x t -> Abs x t
        AppF t u -> App t u

type AnnTerm = Cofree TermF

enannote :: (TermF (Term, AnnTerm ann) -> AnnTerm ann) -> Term -> AnnTerm ann
enannote = para

deannote :: (AnnTerm ann -> TermF (Either Term (AnnTerm ann))) -> AnnTerm ann -> Term
deannote = apo

