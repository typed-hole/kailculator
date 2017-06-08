{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State  (runStateT)

import           ExpressionTree
import           Parsers
import           Parsing

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
    liftIO . print $ x

calculator :: ( Num a, Read a, Show a
              , MonadIO m
              , MonadState (ExpressionTree a) m
              , MonadError String m
              , MonadPlus m )
              => m ()
calculator = do
    input <- liftIO getLine
    if not . null $ input then
        do
            parseInput input
            evalAndPrint
            calculator
    else
        liftIO $ putStrLn "Byebye!"


main = runStateT (runExceptT $ calculator `catchError` handler) (Value 0)
    where handler msg = do
            liftIO $ putStrLn msg
            calculator `catchError` handler
