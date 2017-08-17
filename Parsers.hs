{-# LANGUAGE FlexibleContexts #-}

module Parsers
( parseNum
, parseNumOp
, parsePartialOpExpr
, parseOpExpr
, parseOpExprs
, parseNumExpr
, parseExpression
, unwrapOpParser ) where

import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class

import ExpressionTree
import Parsing

parseNum :: (Num a, Read a) => Parser a
parseNum = do
    s <- get
    let (digits, s') = span isDigit s
    guard . not . null $ digits
    put s'
    return . read $ digits

parseNumOp :: Num a => Parser (ExpressionTree a -> ExpressionTree a -> ExpressionTree a)
parseNumOp = do
    s <- get
    guard . not . null $ s
    let (op:s') = s
     in do
        put s'
        case op of
            '+' -> return $ Op (+)
            '-' -> return $ Op (-)
            '*' -> return $ Op (*)
            _ -> mzero


parseOpExpr :: (Read a, Num a) => Parser (ExpressionTree a)
parseOpExpr = parseNumExpr <**> parseNumOp <*> parseNumExpr

parseNumExpr :: (Read a, Num a) => Parser (ExpressionTree a)
parseNumExpr = Value <$> parseNum

parsePartialOpExpr :: (Read a, Num a) => Parser (ExpressionTree a -> ExpressionTree a)
parsePartialOpExpr = flip <$> parseNumOp <*> parseNumExpr

parseOpExprs :: (Read a, Num a) => Parser (ExpressionTree a)
parseOpExprs = do
    x <- parseOpExpr
    s <- get
    parseOneMore x s
    where parseOneMore x s    = maybe (noMore x) (oneMore x) (runParser parsePartialOpExpr s)
          noMore                = return
          oneMore x (pop, s)  = put s >> parseOneMore (pop x) s


parseExpression :: (Read a, Num a) => Parser (ExpressionTree a)
parseExpression = parseOpExprs <|> parseNumExpr
          
unwrapOpParser :: Parser (ExpressionTree a) -> String -> Maybe (a, String)
unwrapOpParser p s = do
    (expr, s') <- runParser p s
    return (evaluate expr, s')