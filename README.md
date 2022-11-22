# `exceptiot`

Sometimes, it is nice to write code in `MonadError` to indicate that the code
can throw an exception.

```haskell
foo :: MonadError FooError m => Int -> m Char
```

We can call `foo` in `Either FooError` for a pure test, or a `ExceptT FooError
IO` if we are writing in some monad directly. However, sometimes you can't use
`ExceptT`. One possible reason is performance: `ExceptT` can add overhead.
Another reason is `MonadUnliftIO` - `ExceptT` cannot have an instance for this.

You don't want to *stop* using `MonadError`, but you also can't use `ExceptT`
anymore. What can you do?

```haskell
import Control.Monad.Except.IO

main :: IO ()
main = do
    eres <- runExceptIOT (foo 3)
    case eres of
        Left FooError ->
            putStrLn "got FooError"
        Right c ->
            putChar c
```

You can replace `ExceptT` with `ExceptIOT`, and all exception use will switch to
`IO` based exceptions instead of `Either`-based values. This allows you to use
`MonadUnliftIO` without fear.
