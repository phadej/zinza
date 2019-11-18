{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
module Zinza.Check (check) where

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

-------------------------------------------------------------------------------
-- Nodes
-------------------------------------------------------------------------------

check :: forall a m. (Zinza a, ThrowRuntime m) => Nodes Var -> Either CompileError (a -> m String)
check nodes = case toType (Proxy :: Proxy a) of
    rootTy@(TyRecord env) -> do
        nodes' <- flip (traverse . traverseWithLoc) nodes $ \loc var ->
            case M.lookup var env of
                Nothing -> Left (UnboundTopLevelVar loc var)
                Just _  -> Right (EField (L loc (EVar (L loc (Identity rootTy)))) (L loc var))

        run <- checkNodes (map (>>== id) nodes')
        return $ fmap ($ "") . run . Identity . toValue

    rootTy -> throwRuntime (NotRecord zeroLoc rootTy)

checkNodes
    :: (Indexing v i, ThrowRuntime m)
    => Nodes (i Ty)  -- ^ nodes with root object
    -> Either CompileError (v Value -> m ShowS)
checkNodes nodes = do
    nodes' <- traverse checkNode nodes
    return $ \val -> do
        ss <- traverse ($ val) nodes'
        return (foldr (.) id ss)

checkNode :: (Indexing v i, ThrowRuntime m) => Node (i Ty) -> Either CompileError (v Value -> m ShowS)
checkNode NComment = return $ \_val -> return id
checkNode (NRaw s) = return $ \_val -> return (showString s)
checkNode (NIf expr nodes) = do
    b' <- checkBool expr
    nodes' <- checkNodes nodes
    return $ \ctx -> do
        b'' <- b' ctx
        if b''
        then nodes' ctx
        else return id
checkNode (NExpr e) = do
    e' <- checkString e
    return $ \ctx -> do
        s <- e' ctx
        return $ showString s
checkNode (NFor _v expr nodes) = do
    (expr', ty) <- checkList expr
    nodes' <- checkNodes (fmap (fmap (maybe (Here ty) There)) nodes)
    return $ \ctx -> do
        xs <- expr' ctx
        pieces <- for xs $ \x -> nodes' (x ::: ctx)
        return $ foldr (.) id pieces

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

checkList :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Either CompileError (v Value -> m [Value], Ty)
checkList e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyList _ ty' -> return (e' >=> go, ty')
        _            -> throwRuntime (NotList l ty)
  where
    go (VList xs) = return xs
    go x          = throwRuntime (NotList l (valueType x))

checkBool :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Either CompileError (v Value -> m Bool)
checkBool e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyBool -> return (e' >=> go)
        _      -> throwRuntime (NotBool l ty)
  where
    go (VBool b) = return b
    go x         = throwRuntime (NotBool l (valueType x))

checkString :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Either CompileError (v Value -> m String)
checkString e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyString _ -> return (e' >=> go)
        _          -> throwRuntime (NotString l ty)
  where
    go (VString b) = return b
    go x           = throwRuntime (NotString l (valueType x))

checkType :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Either CompileError (v Value -> m Value, Ty)
checkType (L _ (EVar (L _ i))) = return (\v -> return (fst (index v i)), extract i)
checkType (L _ (ENot b)) = do
    b' <- checkBool b
    return (fmap (VBool . not) . b', TyBool)
checkType (L eLoc (EField e (L nameLoc name))) = do
    (e', ty) <- checkType e
    case ty of
        TyRecord tym -> case M.lookup name tym of
            Just (_sel, tyf) -> return (e' >=> go, tyf)
            Nothing          -> throwRuntime (FieldNotInRecord nameLoc name ty)
        _ -> throwRuntime (NotRecord eLoc ty)
  where
    go x@(VRecord r) = case M.lookup name r of
        Just y  -> return y
        Nothing -> throwRuntime (FieldNotInRecord nameLoc name (valueType x))
    go x = throwRuntime (NotRecord eLoc (valueType x))
