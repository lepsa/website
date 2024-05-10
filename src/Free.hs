{-# LANGUAGE GADTs #-}

module Free where

-- Experiments with Free monads, and using them to make a DSL

data Free f a
  = Free (f (Free f a))
  | Pure a

instance (Functor f) => Functor (Free f) where
  fmap f (Free m) = Free $ fmap f <$> m
  fmap f (Pure a) = Pure $ f a

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  (Pure f) <*> (Pure a) = Pure $ f a
  (Pure f) <*> (Free a) = Free $ fmap f <$> a
  (Free f) <*> a        = Free $ (<*> a) <$> f

instance (Functor f) => Monad (Free f) where
  (Pure a) >>= f = f a
  (Free a) >>= f = Free $ (>>= f) <$> a

data Ast a where
  Void :: a -> Ast a
  Literal :: t -> (t -> a) -> Ast a
  Math :: Math a -> Ast a
  Show_ :: (Show t) => t -> (String -> a) -> Ast a
  Print :: String -> a -> Ast a

instance Functor Ast where
  fmap f (Void a)      = Void (f a)
  fmap f (Math m)      = Math $ f <$> m
  fmap f (Literal t a) = Literal t (f . a)
  fmap f (Show_ t a)   = Show_ t (f . a)
  fmap f (Print t a)   = Print t (f a)

data Math a where
  Add :: (Num t) => t -> t -> (t -> a) -> Math a
  Sub :: (Num t) => t -> t -> (t -> a) -> Math a
  Mul :: (Num t) => t -> t -> (t -> a) -> Math a
  Div :: (Fractional t) => t -> t -> (t -> a) -> Math a
  Exp :: (Integral t) => t -> t -> (t -> a) -> Math a

instance Functor Math where
  fmap f (Add x y next) = Add x y $ f . next
  fmap f (Sub x y next) = Sub x y $ f . next
  fmap f (Mul x y next) = Mul x y $ f . next
  fmap f (Div x y next) = Div x y $ f . next
  fmap f (Exp x y next) = Exp x y $ f . next

interpretMaths :: Math a -> a
interpretMaths f = case f of
  Add x y next -> next $ x + y
  Sub x y next -> next $ x - y
  Mul x y next -> next $ x * y
  Div x y next -> next $ x / y
  Exp x y next -> next $ x ^ y

interpretAst :: Ast a -> IO a
interpretAst f = case f of
  Void a -> pure a
  Literal t next -> pure $ next t
  Print t next -> do
    print t
    pure next
  Math m -> pure $ interpretMaths m
  Show_ t next -> pure . next $ show t

freeFold :: (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
freeFold _ (Pure a)   = pure a
freeFold nat (Free f) = nat f >>= freeFold nat

interpret :: Free Ast a -> IO a
interpret = freeFold interpretAst

liftF :: (Functor f) => f a -> Free f a
liftF f = Free $ Pure <$> f

void :: Free Ast ()
void = liftF $ Void ()

literal :: t -> Free Ast t
literal t = liftF $ Literal t id

print_ :: String -> Free Ast ()
print_ t = liftF $ Print t ()

add :: (Num a) => a -> a -> Free Ast a
add x y = liftF . Math $ Add x y id

sub :: (Num a) => a -> a -> Free Ast a
sub x y = liftF . Math $ Sub x y id

mul :: (Num a) => a -> a -> Free Ast a
mul x y = liftF . Math $ Mul x y id

div :: (Fractional a) => a -> a -> Free Ast a
div x y = liftF . Math $ Div x y id

exp :: (Integral a) => a -> a -> Free Ast a
exp x y = liftF . Math $ Exp x y id

show_ :: (Show a) => a -> Free Ast String
show_ a = liftF $ Show_ a id

program :: Free Ast ()
program = do
  void
  a <- literal @Double 1.0
  b <- literal 2
  c <- add a b
  c' <- show_ c
  s <- literal "Foo"
  print_ c'
  print_ s
