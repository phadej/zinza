module Zinza.Parser (parseTemplate) where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad       (void, when)
import Data.Char           (isAlphaNum, isLower)
import Data.Maybe          (isJust)
import Text.Parsec
       (eof, getPosition, lookAhead, notFollowedBy, parse, satisfy, try)
import Text.Parsec.Char    (char, space, spaces, string)
import Text.Parsec.Pos     (SourcePos, sourceColumn, sourceLine)
import Text.Parsec.String  (Parser)

import Zinza.Errors
import Zinza.Expr
import Zinza.Node
import Zinza.Pos
import Zinza.Var

-- | Parse template into nodes. No other than syntactic checks are performed.
parseTemplate
    :: FilePath  -- ^ name of the template
    -> String    -- ^ contents of the template
    -> Either ParseError (Nodes Var)
parseTemplate input contents
    = either (Left . ParseError . show) Right
    $ parse (nodesP <* eof) input contents

-------------------------------------------------------------------------------
-- Location
-------------------------------------------------------------------------------

toLoc :: SourcePos -> Loc
toLoc p = Loc (sourceLine p) (sourceColumn p)

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

varP :: Parser Var
varP = (:) <$> satisfy isLower <*> many (satisfy isVarChar)

locVarP :: Parser (Located Var)
locVarP = do
    pos <- getPosition
    L (toLoc pos) <$> varP

isVarChar :: Char -> Bool
isVarChar c = isAlphaNum c || c == '_'

nodeP :: Parser (Node Var)
nodeP = directiveP <|> exprNodeP <|> newlineN <|> rawP

nodesP :: Parser (Nodes Var)
nodesP = many nodeP

newlineN :: Parser (Node Var)
newlineN = NRaw . pure <$> char '\n'

rawP :: Parser (Node Var)
rawP = mk <$> some rawCharP <*> optional (char '\n') where
    rawCharP   = notBrace <|> try (char '{' <* lookAhead notSpecial)
    notBrace   = satisfy $ \c -> c /= '{' && c /= '\n'
    notSpecial = satisfy $ \c -> c /= '{' && c /= '\n' && c /= '%'

    mk s Nothing  = NRaw s
    mk s (Just c) = NRaw (s ++ [c])

exprNodeP :: Parser (Node Var)
exprNodeP = do
    _ <- try (string "{{")
    spaces
    expr <- exprP
    spaces
    _ <- string "}}"
    return (NExpr expr)

exprP :: Parser (Expr Var)
exprP =  do
    b <- optional (char '!')
    v <- locVarP
    vs <- many (char '.' *> varP)
    let expr = foldl EField (EVar v) vs
    return $
        if isJust b
        then ENot expr
        else expr

directiveP :: Parser (Node Var)
directiveP = forP <|> ifP

spaces1 :: Parser ()
spaces1 = space *> spaces

open :: String -> Parser Bool
open n = do
    pos <- getPosition
    _ <- try $ string "{%" *> spaces *> string n *> spaces
    return $ sourceColumn pos == 1  -- parsec counts pos from 1, not zero.

close :: String -> Parser ()
close n = do
    on0 <- open ("end" ++ n)
    close' on0

close' :: Bool -> Parser ()
close' on0 = do
    _ <- string "%}"
    when on0 $ void $ optional (char '\n')

forP :: Parser (Node Var)
forP = do
    on0 <- open "for"
    var <- varP
    spaces1
    _ <- string "in"
    notFollowedBy $ satisfy isAlphaNum
    spaces1
    expr <- exprP
    spaces1
    close' on0
    ns <- nodesP
    close "for"
    return $ NFor var expr (abstract1 var <$> ns)

ifP :: Parser (Node Var)
ifP = do
    on0 <- open "if"
    expr <- exprP
    spaces
    close' on0
    ns <- nodesP
    close "if"
    return $ NIf expr ns
