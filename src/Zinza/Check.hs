{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zinza.Check where

import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import Control.Monad ((>=>))
import Data.Functor.Identity (Identity (..))

import qualified Data.Map.Strict as M

import Zinza.Class
import Zinza.Expr
import Zinza.Node
import Zinza.Errors
import Zinza.Indexing
import Zinza.Var
import Zinza.Type
import Zinza.Value

check :: forall a m. (Zinza a, ThrowRuntime m) => Nodes Var -> Either CompileError (a -> m String)
check nodes = case toType (Proxy :: Proxy a) of
    rootTy@(TyRecord env) -> do
        nodes' <- flip (traverse . traverse) nodes $ \var -> case M.lookup var env of
            Nothing -> Left (UnboundTopLevelVar var)
            Just _  -> return (EField (EVar (Identity rootTy)) var)

        run <- check1 (map (>>== id) nodes')
        return $ fmap ($ "") . run . Identity . toValue

    _ -> throwRuntime NotRecord

check1
    :: (Indexing v i, ThrowRuntime m)
    => Nodes (i Ty)  -- ^ nodes with root object
    -> Either CompileError (v Value -> m ShowS)
check1 nodes = do
    nodes' <- traverse check2 nodes
    return $ \val -> do
        ss <- traverse ($ val) nodes'
        return (foldr (.) id ss)

check2 :: (Indexing v i, ThrowRuntime m) => Node (i Ty) -> Either CompileError (v Value -> m ShowS)
check2 (NRaw s) = return $ \_val -> return (showString s)
check2 (NIf _ _) = return $ \_ -> return (showString "unimplemented if")
check2 (NExpr e) = do
    e' <- checkString e
    return $ \ctx -> do
        s <- e' ctx
        return $ showString s
check2 (NFor _v expr nodes) = do
    (expr', ty) <- checkList expr
    nodes' <- check1 (fmap (fmap (maybe (Here ty) There)) nodes)
    return $ \ctx -> do
        xs <- expr' ctx
        pieces <- for xs $ \x -> nodes' (x ::: ctx)
        return $ foldr (.) id pieces

{-
        VList xs -> foldr (.) id <$> traverse nodes' xs
        v        -> Left (ForArgumentNotList v)
-}

checkList :: (Indexing v i, ThrowRuntime m) => Expr (i Ty) -> Either CompileError (v Value -> m [Value], Ty)
checkList e = do
    (e', ty) <- checkType e
    case ty of
        TyList ty' -> return (e' >=> go, ty')
        _          -> throwRuntime NotList
  where
    go (VList xs) = return xs
    go _          = throwRuntime NotList

checkBool :: (Indexing v i, ThrowRuntime m) => Expr (i Ty) -> Either CompileError (v Value -> m Bool)
checkBool e = do
    (e', ty) <- checkType e
    case ty of
        TyBool -> return (e' >=> go)
        _      -> throwRuntime NotBool
  where
    go (VBool b) = return b
    go _         = throwRuntime NotBool

checkString :: (Indexing v i, ThrowRuntime m) => Expr (i Ty) -> Either CompileError (v Value -> m String)
checkString e = do
    (e', ty) <- checkType e
    case ty of
        TyString -> return (e' >=> go)
        _        -> throwRuntime NotString
  where
    go (VString b) = return b
    go _           = throwRuntime NotString

checkType :: (Indexing v i, ThrowRuntime m) => Expr (i Ty) -> Either CompileError (v Value -> m Value, Ty)
checkType (EVar i) = return (\v -> return (fst (index v i)), extract i)
checkType (ENot b) = do
    b' <- checkBool b
    return (fmap (VBool . not) . b', TyBool)
checkType (EField e n) = do
    (e', ty) <- checkType e
    case ty of
        TyRecord tym -> case M.lookup n tym of
            Just (_sel, tyf) -> return (e' >=> go, tyf)
            Nothing          -> throwRuntime (FieldNotInRecord n)
        _ -> throwRuntime NotRecord
  where
    go (VRecord r) = case M.lookup n r of
        Just x  -> return x
        Nothing -> throwRuntime (FieldNotInRecord n)
    go _ = throwRuntime NotRecord


