module Product where

import Prelude

import Bifunctor (class Bifunctor)

data ProductT a b = Cons a b

p1 :: forall a b. ProductT a b -> a
p1 (Cons a _) = a

p2 :: forall a b. ProductT a b -> b
p2 (Cons _ b) = b

genU
  :: forall x a b
   . (x -> a)
  -> (x -> b)
  -> x
  -> ProductT a b
genU x1 x2 x = Cons (x1 x) (x2 x)

class ProductC product a b | product -> a b where
  toProductT :: product -> ProductT a b
  fromProductT :: ProductT a b -> product

instance Bifunctor ProductT where
  bimap f g (Cons a b) = Cons (f a) (g b)

class Vartheta product a b | product -> a b where
  vartheta
    :: forall x
     . (x -> product)
    -> ProductT (x -> a) (x -> b) --Productは集合の意味での直積として使っている
  varthetaInv
    :: forall x
     . ProductT (x -> a) (x -> b)
    -> (x -> product)

instance ProductC (ProductT a b) a b where
  toProductT = identity
  fromProductT = identity

else instance Vartheta product a b => ProductC product a b where
  toProductT p = Cons (a unit) (b unit)
    where
    Cons a b = vartheta (const p)
  fromProductT (Cons a b) = varthetaInv (Cons (const a) (const b)) unit

instance ProductC product a b => Vartheta product a b where
  vartheta = f
    where
    f p = Cons a b
      where
      a x = p1 $ toProductT (p x)
      b x = p2 $ toProductT (p x)
  varthetaInv = fInv
    where
    fInv (Cons a b) x = fromProductT $ Cons (a x) (b x)

testHomFunctor
  :: forall a b x
   . (x -> ProductT a b)
  -> ProductT (x -> a) (x -> b)
testHomFunctor p = Cons a b
  where
  a x = p1 $ p x
  b x = p2 $ p x

testHomFunctorInv
  :: forall a b x
   . ProductT (x -> a) (x -> b) -> (x -> ProductT a b)
testHomFunctorInv (Cons a b) x = Cons (a x) (b x)
