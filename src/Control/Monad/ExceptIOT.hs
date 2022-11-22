{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Control.Monad.ExceptIOT where

import UnliftIO
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except


newtype ExceptIOT e m a = ExceptIOT { unsafeRunExceptIOT :: m a }
    deriving newtype
        (Functor, Applicative, Monad, MonadIO, MonadPlus, Alternative,
        Semigroup, Monoid, MonadUnliftIO, MonadReader r, MonadState s, MonadWriter w)

instance (MonadUnliftIO m, Exception e) => MonadError e (ExceptIOT e m) where
    throwError = throwIO
    catchError = catch
