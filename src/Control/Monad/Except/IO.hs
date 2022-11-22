{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Control.Monad.Except.IO
    ( ExceptIOT (..)
    , runExceptIOT
    , module Control.Monad.Error.Class
    , modifyError
    ) where

import Control.Monad.Catch hiding (try, catch)
import UnliftIO
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error.Class hiding (modifyError)
import qualified Control.Monad.Except as Except

-- | This type is useful for providing a 'MonadError' constraint to an 'IO'
-- action for a given type. It can replace 'ExceptT'.
--
-- Note that 'catchError' will use the behavior from "UnliftIO" - so catch won't
-- catch an asynchronous exception.
newtype ExceptIOT e m a = ExceptIOT { unsafeRunExceptIOT :: m a }
    deriving newtype
        ( Functor, Applicative, Monad, MonadIO, MonadPlus, Alternative
        , Semigroup, Monoid, MonadUnliftIO, MonadReader r, MonadState s
        , MonadWriter w, MonadFix, MonadFail, MonadThrow, MonadCatch, MonadMask
        )

instance (MonadUnliftIO m, Exception e) => MonadError e (ExceptIOT e m) where
    throwError = throwIO
    catchError = catch

runExceptIOT :: (Exception e, MonadUnliftIO m) => ExceptIOT e m a -> m (Either e a)
runExceptIOT = try . unsafeRunExceptIOT

-- | Like 'Except.modifyError', but it selects the 'ExceptIOT' instance for 'IO'
-- exceptions instead of the 'ExceptT' instance with an 'Either' error.
modifyError
    :: (Exception e, MonadUnliftIO m, MonadError e' m)
    => (e -> e') -> ExceptIOT e m a -> m a
modifyError f action =
    runExceptIOT action >>= either (throwError . f) pure
