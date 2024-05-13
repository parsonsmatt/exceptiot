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
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Error.Class hiding (modifyError)
import Control.Monad.Fix
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

-- |
--
-- @since 0.1.0.0
instance (MonadCatch m, Exception e) => MonadError e (ExceptCatchT e m) where
    throwError = throwM
    catchError = catch

-- | Run an 'ExceptCatchT' action. This will catch any thrown @e@ exceptions,
-- regardless of whether you used 'throwM' or 'throwError'.
--
-- Any exception that is not mentioned in @e@ will be thrown - this does not
-- catch all exceptions!
--
-- @since 0.1.0.0
runExceptCatchT :: (Exception e, MonadCatch m) => ExceptCatchT e m a -> m (Either e a)
runExceptCatchT = try . unsafeRunExceptCatchT

-- | Like 'Control.Monad.Except.modifyError', but it selects the 'ExceptCatchT' instance for 'IO'
-- exceptions instead of the 'ExceptT' instance with an 'Either' error.
--
-- @since 0.1.0.0
modifyError
    :: (Exception e, MonadCatch m, MonadError e' m)
    => (e -> e') -> ExceptCatchT e m a -> m a
modifyError f action =
    runExceptCatchT action >>= either (throwError . f) pure
