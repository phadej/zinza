{-# LANGUAGE TypeOperators, StandaloneKindSignatures, RankNTypes, GADTs, DataKinds, KindSignatures, PolyKinds #-}
module Zinza where

import Data.Kind (Type)
import GHC.TypeLits (Symbol, SSymbol)

type TyVar :: [Type] -> Type
data TyVar kctx where
    TZ ::               TyVar (a : kctx)
    TS :: TyVar kctx -> TyVar (b : kctx)

data Ty kctx
    = TyVar (TyVar kctx)
    | Bool
    | Str
    | Fun (Ty kctx) (Ty kctx)

data Co kctx
    = HasFieldCo (TyVar kctx) Symbol (Ty kctx)

type Var :: forall (kctx :: [Type]). [Ty kctx] -> Ty kctx -> Type
data Var ctx a where
    VZ ::              Var (a : ctx) a
    VS :: Var ctx a -> Var (b : ctx) a

type Expr :: forall (kctx :: [Type]). [Co kctx] -> [Ty kctx] -> Ty kctx -> Type
data Expr (co :: [Co kctx]) ctx ty where
    Var :: Var ctx ty -> Expr co ctx ty
    App :: Expr co ctx (Fun a b) -> Expr co ctx a -> Expr co ctx b
    Sel :: Expr co ctx ('TyVar ty) -> SSymbol field -> ( {- HasFieldCo dictionary -}) -> Expr co ctx b
