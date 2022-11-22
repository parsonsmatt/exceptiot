{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Except.Catch
    ( ExceptCatchT (..)
    , runExceptCatchT
    , module Control.Monad.Error.Class
    , modifyError
    ) where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Error.Class hiding (modifyError)
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- | This type is useful for translating a 'MonadError' constraint into
-- 'MonadCatch'. This type does not have an 'Either' return, which means we can
-- provide a 'MonadUnliftIO' instance.
--
-- @since 0.1.0.0
newtype ExceptCatchT e m a = ExceptCatchT { unsafeRunExceptCatchT :: m a }
    deriving newtype
        ( Functor, Applicative, Monad, MonadIO, MonadPlus, Alternative
        , Semigroup, Monoid, MonadUnliftIO, MonadReader r, MonadState s
        , MonadWriter w , MonadFix, MonadFail, MonadThrow, MonadCatch, MonadMask
        )

instance (MonadCatch m, Exception e) => MonadError e (ExceptCatchT e m) where
    throwError = throwM
    catchError = catch

runExceptCatchT :: (Exception e, MonadCatch m) => ExceptCatchT e m a -> m (Either e a)
runExceptCatchT = try . unsafeRunExceptCatchT

-- | Like 'Except.modifyError', but it selects the 'ExceptCatchT' instance for 'IO'
-- exceptions instead of the 'ExceptT' instance with an 'Either' error.
modifyError
    :: (Exception e, MonadCatch m, MonadError e' m)
    => (e -> e') -> ExceptCatchT e m a -> m a
modifyError f action =
    runExceptCatchT action >>= either (throwError . f) pure
