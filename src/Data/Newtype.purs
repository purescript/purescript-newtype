module Data.Newtype where

import Prelude

class Newtype t a | t -> a where
  wrap :: a -> t
  unwrap :: t -> a

op :: forall t a. Newtype t a => (a -> t) -> t -> a
op _ = unwrap

ala
  :: forall f t a s b
   . (Functor f, Newtype t a, Newtype s b)
  => (a -> t)
  -> ((b -> s) -> f t)
  -> f a
ala _ f = map unwrap (f wrap)

alaF
  :: forall f g t a s b
   . (Functor f, Functor g, Newtype t a, Newtype s b)
  => (a -> t)
  -> (f t -> g s)
  -> f a
  -> g b
alaF _ f = map unwrap <<< f <<< map wrap

under
  :: forall t a s b
   . (Newtype t a, Newtype s b)
  => (a -> t)
  -> (t -> s)
  -> (a -> b)
under _ f = unwrap <<< f <<< wrap

over
  :: forall t a s b
   . (Newtype t a, Newtype s b)
  => (a -> t)
  -> (a -> b)
  -> t
  -> s
over _ f = wrap <<< f <<< unwrap

underF
  :: forall f t a s b
   . (Newtype t a, Newtype s b, Functor f)
  => (a -> t)
  -> (f t -> f s)
  -> f a
  -> f b
underF _ f = map unwrap <<< f <<< map wrap

overF
  :: forall f t a s b
   . (Newtype t a, Newtype s b, Functor f)
  => (a -> t)
  -> (f a -> f b)
  -> f t
  -> f s
overF _ f = map wrap <<< f <<< map unwrap
