{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
module Zinza.Check where

import Control.Monad         ((>=>))
import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (..))
import Data.Traversable      (for)

import qualified Data.Map.Strict as M

import Zinza.Class
import Zinza.Errors
import Zinza.Expr
import Zinza.Indexing
import Zinza.Node
import Zinza.Pos
import Zinza.Type
import Zinza.Value
import Zinza.Var

check :: forall a m. (Zinza a, ThrowRuntime m) => Nodes Var -> Either CompileError (a -> m String)
check nodes = case toType (Proxy :: Proxy a) of
    rootTy@(TyRecord env) -> do
        nodes' <- flip (traverse . traverseWithLoc) nodes $ \(L loc var) ->
            case M.lookup var env of
                Nothing -> Left (UnboundTopLevelVar loc var)
                Just _  -> Right (EField (EVar (L loc (Identity rootTy))) (L loc var))

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
checkType (EVar (L _ i)) = return (\v -> return (fst (index v i)), extract i)
checkType (ENot b) = do
    b' <- checkBool b
    return (fmap (VBool . not) . b', TyBool)
checkType (EField e (L nameLoc name)) = do
    (e', ty) <- checkType e
    case ty of
        TyRecord tym -> case M.lookup name tym of
            Just (_sel, tyf) -> return (e' >=> go, tyf)
            Nothing          -> throwRuntime (FieldNotInRecord nameLoc name ty)
        _ -> throwRuntime NotRecord
  where
    go x@(VRecord r) = case M.lookup name r of
        Just y  -> return y
        Nothing -> throwRuntime (FieldNotInRecord nameLoc name (valueType x))
    go _ = throwRuntime NotRecord
