{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module has the same shape as "Zinza.Check".
module Zinza.Module (
    checkModule,
    ModuleConfig (..),
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), get, modify')
import Data.Foldable             (traverse_)
import Data.Maybe                (fromMaybe)
import Data.Proxy                (Proxy (..))

import qualified Data.Map.Strict as M

import Zinza.Class
import Zinza.Errors
import Zinza.Expr
import Zinza.Node
import Zinza.Pos
import Zinza.Type
import Zinza.Var

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

type M = StateT S (Either CompileError)

data S = S
    { sOutput :: [(Int, String)] -> [(Int, String)]
    , sIndent :: Int
    , sVars   :: Int
    , sBlocks :: M.Map Var HsExpr
    }

tell :: String -> M ()
tell str = modify' $ \s -> s { sOutput = sOutput s . ((sIndent s, str) :)}

indented :: M a -> M a
indented m = do
    modify' $ \s -> s { sIndent = succ (sIndent s) }
    x <- m
    modify' $ \s -> s { sIndent = pred (sIndent s) }
    return x

newVar :: String -> M Selector
newVar name = do
    n <- sVars <$> get
    modify' $ \s -> s { sVars = succ n }
    return ("z_var" ++ show n ++ "_" ++ name)


flatten :: [(Int, String)] -> String
flatten xs = unlines
    [ replicate (i + i) ' ' ++ str
    | (i, str) <- xs
    ]

-------------------------------------------------------------------------------
-- ModuleConfig
-------------------------------------------------------------------------------

-- | Configuration for module rendering
data ModuleConfig a = ModuleConfig
    { mcHeader :: [String]  -- ^ module header
    , mcRender :: String    -- ^ name of the function
    }
  deriving Show

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

checkModule
    :: forall a. Zinza a
    => ModuleConfig a
    -> Nodes Var
    -> Either CompileError String
checkModule mc nodes =  case toType (Proxy :: Proxy a) of
    TyRecord env -> do
        nodes' <- flip (traverse .traverseWithLoc) nodes $ \loc var ->
            case M.lookup var env of
                Nothing        -> Left (UnboundTopLevelVar loc var)
                Just (sel, ty) -> Right (rootExpr `access` sel, ty)

        ((), S out _ _ _) <- runStateT (header *> indented (checkNodes nodes')) (S id 0 0 M.empty)
        return (flatten (out []))
    rootTy -> throwRuntime (NotRecord zeroLoc rootTy)
  where
    header = do
        traverse_ tell (mcHeader mc)
        tell $ mcRender mc ++ " " ++ displayHsExpr rootExpr ++ " = execWriter $ do"

    rootExpr :: HsExpr
    rootExpr = hsVar "z_root"

checkNodes :: Nodes (HsExpr, Ty) -> M ()
checkNodes = traverse_ checkNode

checkNode :: Node (HsExpr, Ty) -> M ()
checkNode NComment = return ()
checkNode (NRaw s) = tell $ "tell " ++ show s
checkNode (NExpr expr) = do
    expr' <- lift $ checkString expr
    tell $ "tell " ++ displayHsExpr expr'
checkNode (NIf expr xs ys) = do
    expr' <- lift $ checkBool expr
    tell $ "if " ++ displayHsExpr expr'
    tell "then do"
    indented $ do
        resettingBlocks $ checkNodes xs
        tell $ "return ()"
    tell "else do"
    indented $ do
        resettingBlocks $ checkNodes ys
        tell $ "return ()"
checkNode (NFor v expr nodes) = do
    v' <- newVar v
    (expr', ty) <- lift (checkList expr)
    tell $ "forM_ " ++ displayHsExpr expr' ++ " $ \\" ++ v' ++ " -> do"
    indented $ checkNodes $ map (fmap (fromMaybe (hsVar v', ty))) nodes
checkNode (NDefBlock l n nodes) = do
    blocks <- fmap sBlocks get
    if M.member n blocks
    then lift (Left (UnboundUseBlock l n))
    else do
        v' <- fmap hsVar (newVar n)
        tell $ "let"
        indented $ do
            tell $ displayHsExpr v' ++ " = do"
            indented $ do
                checkNodes nodes
                tell $ "return ()"
        modify' $ \s' -> s' { sBlocks = M.insert n v' blocks }
checkNode (NUseBlock l n) = do
    S _ _ _ blocks <- get
    case M.lookup n blocks of
        Nothing -> lift (Left (UnboundUseBlock l n))
        Just block -> tell $ displayHsExpr block

resettingBlocks :: M a -> M a
resettingBlocks m = do
    s <- get
    x <- m
    modify' $ \s' -> s' { sBlocks = sBlocks s }
    return x

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

checkList :: LExpr (HsExpr, Ty) -> Either CompileError (HsExpr, Ty)
checkList e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyList sel ty' -> return (e' `accessMaybe` sel, ty')
        _ ->  throwRuntime (NotList l ty)

checkString :: LExpr (HsExpr, Ty) -> Either CompileError HsExpr
checkString e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyString sel -> return (e' `accessMaybe` sel)
        _            -> throwRuntime (NotString l ty)

checkBool :: LExpr (HsExpr, Ty) -> Either CompileError HsExpr
checkBool e@(L l _) = do
    (e', ty) <- checkType e
    case ty of
        TyBool -> return e'
        _      -> throwRuntime (NotBool l ty)

checkType :: LExpr (HsExpr, Ty) -> Either CompileError (HsExpr, Ty)
checkType (L _ (EVar (L _ x))) = return x
checkType (L eLoc (EField e (L nameLoc name))) =do
    (e', ty) <- checkType e
    case ty of
        TyRecord tym -> case M.lookup name tym of
            Just (sel, tyf) -> return (e' `access` sel, tyf)
            Nothing         -> throwRuntime (FieldNotInRecord nameLoc name ty)
        _ -> throwRuntime (NotRecord eLoc ty)
checkType (L eLoc (EApp f@(L fLoc _) x)) = do
    (f', fTy) <- checkType f
    (x', xTy) <- checkType x
    case fTy of
        TyFun xTy' yTy | xTy == xTy' -> do
            return (HsApp f' x', yTy)
        TyFun xTy' _ -> throwRuntime (FunArgDontMatch fLoc xTy xTy')
        _            -> throwRuntime (NotFunction eLoc fTy)
