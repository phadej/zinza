module Zinza.Writer (
    Writer,
    execWriter,
    tell,
    ) where

-- | 'String' writer monad.
newtype Writer a = Writer { unWriter :: ShowS -> (ShowS, a) }

-- | Get the written string.
execWriter :: Writer a -> String
execWriter w = fst (unWriter w id) ""

-- | Tell 'String'.
tell :: String -> Writer ()
tell x = Writer $ \s -> (s . showString x, ())

instance Functor Writer where
    fmap f m = Writer $ \s -> fmap f (unWriter m s)

instance Applicative Writer where
    pure x = Writer $ \s -> (s, x)
    {-# INLINE pure #-}

    x *> y = Writer $ \s1 ->
        let (s2, _) = unWriter x s1
        in unWriter y s2
    {-# INLINE (*>) #-}

    x <* y = Writer $ \s1 ->
        let (s2, x') = unWriter x s1
            (s3, _)  = unWriter y s2
            in (s3, x')
    {-# INLINE (<*) #-}

    f <*> x = Writer $ \s1 ->
        let (s2, f') = unWriter f s1
            (s3, x') = unWriter x s2
            in (s3, f' x')
    {-# INLINE (<*>) #-}

instance Monad Writer where
    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

    m >>= k = Writer $ \s1 ->
        let (s2, x) = unWriter m s1
        in unWriter (k x) s2
    {-# INLINE (>>=) #-}
