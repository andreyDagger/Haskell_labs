module HW1.Exam (
    Writer(..),
    fac,
) where

-- data MyMaybe a = MyNothing | MyJust a
-- data MyEither a b = MyLeft a | MyRight b
-- data L a = E | C a (L a)

-- foo :: L String
-- foo = C "1" (C "2" E)

-- newtype UserName = UserName String

-- class MyEq a where
--     (==) :: a -> a -> Bool

-- class MyEq a => MyOrd a where
--     (<) :: a -> a -> Bool
--     (<=) :: a -> a -> Bool
--     (>) :: a -> a -> Bool
--     (>=) :: a -> a -> Bool

-- class MyNum a where
--     (+), (*), (-) :: a -> a -> a
--     fromInteger :: Integer -> a
--     negate :: a -> a
--     abs :: a -> a
--     signum :: a -> a

-- data Compl = Compl Integer Integer
--  deriving (Show, Eq)

-- instance MyNum Compl where
--     (Compl a1 b1) + (Compl a2 b2) = Compl (a1 Prelude.+ b1) (a2 Prelude.+ b2)
--     (Compl a1 b1) - (Compl a2 b2) = Compl (a1 Prelude.- b1) (a2 Prelude.- b2)
--     (Compl a1 b1) * (Compl a2 b2) = Compl (a1 Prelude.* b1 Prelude.- a2 Prelude.* b2) (a1 Prelude.* b2 Prelude.+ a2 Prelude.* b1)
--     fromInteger a = Compl a 0
--     negate (Compl a b) = Compl (Prelude.negate a) (Prelude.negate b)


-- myFoldl :: (b -> a -> b) -> [a] -> b -> b
-- myFoldl _ [] x = x
-- myFoldl f (x:xs) y = myFoldl f xs (f y x)

-- myFoldr :: (a -> b -> b) -> [a] -> b -> b
-- myFoldr _ [] x = x
-- myFoldr f (x:xs) y = f x (myFoldr f xs y)

-- class MyFunctor t where
--     myFmap :: (a -> b) -> t a -> t b

-- instance MyFunctor MyMaybe where
--     myFmap f (MyJust a) = MyJust (f a)
--     myFmap _ MyNothing = MyNothing

-- class MyFunctor t => MyApplicative t where
--     pure :: a -> t a
--     (<*>) :: t (a -> b) -> t a -> t b

-- instance MyApplicative MyMaybe where
--     pure = MyJust
--     MyNothing <*> _ = MyNothing
--     _ <*> MyNothing = MyNothing
--     (MyJust f) <*> (MyJust a) = MyJust (f a)

-- class MyFunctor t => MyMonad t where
--     return :: a -> t a
--     (>>=) :: t a -> (a -> t b) -> t b
--     join :: t (t a) -> t a

-- instance MyMonad MyMaybe where
--     return = MyJust
--     MyNothing >>= _ = MyNothing
--     (MyJust a) >>= f = f a
--     join MyNothing = MyNothing
--     join (MyJust MyNothing) = MyNothing
--     join (MyJust (MyJust a)) = MyJust a

-- newtype MyState s a = MyState { myRunState :: s -> (a, s) }

-- instance MyFunctor (MyState s) where
--     myFmap f (MyState rs) = MyState (\s -> case rs s of
--                                            (a, ss) -> (f a, ss)
--                                            )

-- instance MyMonad (MyState s) where
--     return a = MyState (\s -> (a, s))
--     join (MyState rs) = MyState (\s ->
--         let (MyState rs1, s1) = rs s
--             (a2, s2) = rs1 s1
--             in (a2, s2)
--         )
--     (>>=) (MyState rs) f = MyState (\s ->
--         let (a1, s1) = rs s
--             (MyState rs1) = f a1
--         in rs1 s1)

-- newtype MyReader e a = MyReader { myRunReader :: e -> a }

-- instance MyFunctor (MyReader e) where
--     myFmap f (MyReader r) = MyReader (f . r)

-- instance MyMonad (MyReader e) where
--     return a = MyReader (const a)
--     join (MyReader r1) = MyReader (\e ->
--         let (MyReader r2) = r1 e
--         in r2 e)
--     (>>=) (MyReader r1) f = MyReader (\e ->
--         let (MyReader r2) = f (r1 e)
--         in r2 e)

newtype Writer w a = Writer (a, w)

instance Functor (Writer w) where
    fmap f (Writer r) =
        let (a1, w1) = r
        in Writer (f a1, w1)

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (Writer (a1, w1)) <*> (Writer (a2, w2)) = Writer (a1 a2, w1 <> w2)

instance (Monoid w) => Monad (Writer w) where
    (>>=) (Writer (a1, w1)) f =
        let (Writer (a2, w2)) = f a1
        in Writer (a2, w1 <> w2)


fac :: Int -> Writer String Int
fac n
 | n <= 0 = return 1
 | otherwise = fac (n - 1) >>= (\val -> Writer (val * n, "*" ++ show n))

data MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
    fmap f (MaybeIO x) = MaybeIO (fmap (fmap f) x)

instance Applicative MaybeIO where
    pure x = MaybeIO (return (Just x))
    MaybeIO ioMaybeF <*> MaybeIO ioMaybeA = MaybeIO ( do
        maybeF <- ioMaybeF
        case maybeF of
            Nothing -> return Nothing
            Just f -> fmap (fmap f) ioMaybeA
        )

instance Monad MaybeIO where
    return = pure
    MaybeIO ioMaybeA >>= f = MaybeIO $ do
        maybeA <- ioMaybeA
        case maybeA of
            Nothing -> return Nothing
            Just x -> runMaybeIO (f x)

data A = Foo { foo::Int } | Bar { bar::Int }

class MyFolbadle t where
    myFold :: (Monoid m) => t m -> m
    myFoldMap :: (Monoid m) => (m -> b) -> t m -> m