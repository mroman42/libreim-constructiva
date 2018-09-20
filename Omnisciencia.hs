-- Este código es una adaptación del código de Martín Escardó para
-- los artículos sobre omnisciencia en espacios infinitos del blog
-- "Mathematics and Computation".  Puede leerse más sobre la
-- caracterización de este tipo de espacios en el siguiente artículo.
-- https://www.cs.bham.ac.uk/~mhe/papers/omniscient-2011-07-06.pdf

{-# LANGUAGE FlexibleInstances #-}

module Omnisciencia where

import Control.Monad (ap)
import qualified Data.List

-- Funciones auxiliares.
at = Data.List.genericIndex
n :: Bool -> Int
n True  = 0
n False = 1



-- BÚSQUEDA.
-- Búsqueda exhaustiva sobre un tipo; funciones de búsqueda auxiliares
-- que proporcionan la omnisciencia.
newtype Search a = Search {search :: (a -> Bool) -> a}

forsome :: Search a -> (a -> Bool) -> Bool
forsome s p = p (search s p)

forevery :: Search a -> (a -> Bool) -> Bool
forevery s p = not (forsome s (not . p))





-- ESPACIO DE CANTOR.
-- El espacio de Cantor es el espacio de las funciones de los números
-- naturales a los booleanos.
type Cantor = Integer -> Bool

-- Auxiliar que añade al principio de la lista.
(#) :: Bool -> Cantor -> Cantor
(b # f) 0 = b
(b # f) n = f (n-1)

ecantor1 :: Search Cantor -- Luego implementaremos una versión 2 más eficiente.
ecantor1 = Search $ \p ->
  if forsome ecantor1 (\a -> p (False # a))
  then False # search ecantor1 (\a -> p (False # a))
  else True  # search ecantor1 (\a -> p (True  # a))



-- CONATURAL.
-- Una representación de los números conaturales.
data Conat = Zero | Succ Conat deriving (Eq, Show)

instance Num Conat where
  Zero + y = y
  Succ x + y = Succ (x + y)
  Zero * y = Zero
  Succ x * y = y + (x * y)
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n-1))
  abs x = x
  negate x = x
  signum x = 1

instance Ord Conat where
  Zero     <= b        = True
  (Succ a) <= Zero     = False
  (Succ a) <= (Succ b) = a <= b

infinity :: Conat
infinity = Succ infinity

econat :: Search Conat
econat = Search $ \p -> if p Zero then Zero else Succ $ search econat (p . Succ)



-- MÓNADA.
-- Facilitaremos la creación de nuevos espacios omniscientes
-- implementando una mónada para búsquedas.
join :: Search (Search a) -> Search a
join ss = Search (\p -> search (search ss (`forsome` p)) p)

instance Functor Search where
  fmap f s = Search (\p -> f (search s (p . f)))

instance Applicative Search where
  pure x = Search (const x)
  (<*>)  = ap

instance Monad Search where
  return x = Search (const x)
  s >>= f  = join (fmap f s)


-- Ejemplos de la mónada. En particular, obtenemos una búsqueda más
-- eficiente sobre el espacio de Cantor.
ebit :: Search Bool
ebit = Search ($ True)

einfbool :: Search [Bool]
einfbool = sequence (repeat ebit)

ecantor :: Search Cantor
ecantor = Search $ \f -> at (search einfbool (f . at))


-- Igualdad de funciones.
instance (Eq b) => Eq (Cantor -> b) where
  f == g = forevery ecantor (\u -> f u == g u)
