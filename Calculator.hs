{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (runStateT)
import Text.Read (readMaybe)

import Parsers
import Parsing
import ExpressionTree

parseInput :: ( Num a, Read a
              , MonadState (ExpressionTree a) m
              , MonadError String m)
              => String -> m ()
parseInput "" = return ()
parseInput s = do
    expr <- get
    let parser = parseExpression <|> (($ expr) <$> parsePartialOpExpr)
    let parsed = runParser parser s
    maybe (throwError "Parse error") handle parsed
    where handle (expr', s') = put expr' >> parseInput s'

evalAndPrint :: ( Show a
                , MonadIO m
                , MonadState (ExpressionTree a) m)
                => m ()
evalAndPrint = do
    x <- evaluate <$> get
    put $ Value x
    liftIO . putStrLn . show $ x

calculator :: ( Num a, Read a, Show a
              , MonadIO m
              , MonadState (ExpressionTree a) m
              , MonadError String m
              , MonadPlus m )
              => m ()
calculator = do
    input <- liftIO $ getLine
    case null input of
        False -> do
            parseInput input
            evalAndPrint
            calculator
        True -> liftIO $ putStrLn "Byebye!"


main = runStateT (runExceptT $ calculator `catchError` handler) (Value 0)
    where handler msg = do
            liftIO $ putStrLn msg
            calculator `catchError` handler