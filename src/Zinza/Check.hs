{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Zinza.Check (check) where

import Control.Monad             ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), evalStateT, get, put)
import Data.Functor.Identity     (Identity (..))
import Data.Proxy                (Proxy (..))
import Data.Traversable          (for)

import qualified Data.Map.Strict as Map

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
-- Type
-------------------------------------------------------------------------------

type Check v m = StateT (Map.Map Var (v Value -> m ShowS)) (Either CompileError)

-------------------------------------------------------------------------------
-- Nodes
-------------------------------------------------------------------------------

check :: forall a m. (Zinza a, ThrowRuntime m) => Nodes Var -> Either CompileError (a -> m String)
check nodes = case toType (Proxy :: Proxy a) of
    rootTy@(TyRecord env) -> do
        nodes' <- flip (traverse . traverseWithLoc) nodes $ \loc var ->
            case Map.lookup var env of
                Nothing -> Left (UnboundTopLevelVar loc var)
                Just _  -> Right (EField (L loc (EVar (L loc (Identity rootTy)))) (L loc var))

        run <- evalStateT (checkNodes (map (>>== id) nodes')) Map.empty
        return $ fmap ($ "") . run . Identity . toValue

    rootTy -> throwRuntime (NotRecord zeroLoc rootTy)

checkNodes
    :: (Indexing v i, ThrowRuntime m)
    => Nodes (i Ty)                    -- ^ nodes with root object
    -> Check v m (v Value -> m ShowS)
checkNodes nodes = do
    nodes' <- traverse checkNode nodes
    return $ \val -> do
        ss <- traverse ($ val) nodes'
        return (foldr (.) id ss)

checkNode
    :: (Indexing v i, ThrowRuntime m)
    => Node (i Ty)
    -> Check v m (v Value -> m ShowS)
checkNode NComment = return $ \_val -> return id
checkNode (NRaw s) = return $ \_val -> return (showString s)
checkNode (NIf expr xs ys) = do
    b' <- checkBool expr
    xs' <- resetingState $ checkNodes xs
    ys' <- resetingState $ checkNodes ys
    return $ \ctx -> do
        b'' <- b' ctx
        if b''
        then xs' ctx
        else ys' ctx
checkNode (NExpr e) = do
    e' <- checkString e
    return $ \ctx -> do
        s <- e' ctx
        return $ showString s
checkNode (NFor _v expr nodes) = do
    (expr', ty) <- checkList expr
    blocks <- get
    nodes' <- lift $ evalStateT
        (checkNodes (fmap (fmap (maybe (Here ty) There)) nodes))
        (Map.map (\f (_ ::: xs) -> f xs) blocks)
    return $ \ctx -> do
        xs <- expr' ctx
        pieces <- for xs $ \x -> nodes' (x ::: ctx)
        return $ foldr (.) id pieces
checkNode (NDefBlock l n nodes) = do
    blocks <- get
    if Map.member n blocks
    then lift (Left (ShadowingBlock l n))
    else do
        nodes' <- checkNodes nodes
        put $ Map.insert n nodes' blocks
    return $ \_ -> return id
checkNode (NUseBlock l n) = do
    blocks <- get
    case Map.lookup n blocks of
        Nothing -> lift (Left (UnboundUseBlock l n))
        Just block -> return block

resetingState :: Monad m => StateT s m a -> StateT s m a
resetingState m = do
    s <- get
    x <- m
    put s
    return x

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

checkList :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Check v m (v Value -> m [Value], Ty)
checkList e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyList _ ty' -> return (e' >=> go, ty')
        _            -> throwRuntime (NotList l ty)
  where
    go (VList xs) = return xs
    go x          = throwRuntime (NotList l (valueType x))

checkBool :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Check v m (v Value -> m Bool)
checkBool e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyBool -> return (e' >=> go)
        _      -> throwRuntime (NotBool l ty)
  where
    go (VBool b) = return b
    go x         = throwRuntime (NotBool l (valueType x))

checkString :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Check v m (v Value -> m String)
checkString e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyString _ -> return (e' >=> go)
        _          -> throwRuntime (NotString l ty)
  where
    go (VString b) = return b
    go x           = throwRuntime (NotString l (valueType x))

checkType :: (Indexing v i, ThrowRuntime m) => LExpr (i Ty) -> Check v m (v Value -> m Value, Ty)
checkType (L _ (EVar (L _ i))) =
    return (\v -> return (fst (index v i)), extract i)
checkType (L eLoc (EField e (L nameLoc name))) = do
    (e', ty) <- checkType e
    case ty of
        TyRecord tym -> case Map.lookup name tym of
            Just (_sel, tyf) -> return (e' >=> go, tyf)
            Nothing          -> throwRuntime (FieldNotInRecord nameLoc name ty)
        _ -> throwRuntime (NotRecord eLoc ty)
  where
    go x@(VRecord r) = case Map.lookup name r of
        Just y  -> return y
        Nothing -> throwRuntime (FieldNotInRecord nameLoc name (valueType x))
    go x = throwRuntime (NotRecord eLoc (valueType x))
checkType (L eLoc (EApp f@(L fLoc _) x)) = do
    (f', fTy) <- checkType f
    (x', xTy) <- checkType x
    case fTy of
        TyFun xTy' yTy | xTy == xTy' -> do
            return (go f' x', yTy)
        TyFun xTy' _ -> throwRuntime (FunArgDontMatch fLoc xTy xTy')
        _            -> throwRuntime (NotFunction eLoc fTy)
  where
    go f' x' ctx = do
        f2 <- f' ctx
        x2 <- x' ctx
        case f2 of
            VFun f3 -> either throwRuntime return $ f3 x2
            _    -> throwRuntime (NotFunction eLoc (valueType f2))
