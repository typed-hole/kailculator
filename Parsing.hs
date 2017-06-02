{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parsing
( Parser
, runParser ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.State (runStateT, StateT)
import Text.Read (readMaybe)

newtype Parser a = Parser { unParser :: StateT String Maybe a } deriving ( Functor
                                                                         , Applicative
                                                                         , Monad
                                                                         , MonadState String )

instance Alternative Parser where
    empty = Parser empty
    a <|> b = Parser $ unParser a <|> unParser b

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT . unParser