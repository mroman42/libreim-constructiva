{-# LANGUAGE FlexibleInstances #-}

-- Empezamos definiendo el espacio de Cantor. Incluimos una función
-- auxiliar que añade un elemento al inicio de la secuencia.
type Cantor = Integer -> Bool

(#) :: Bool -> Cantor -> Cantor
(b # f) 0 = b
(b # f) n = f (n-1)

-- Usaremos una definición de epsilon que compondrá una inducción 
-- mutua con otra función que comprueban si existen ejemplos
-- en cada rama del árbol binario.

-- Esta definición de epsilon es debida a Ulrich Berger.
epsilon :: (Cantor -> Bool) -> Cantor
epsilon p =
  if forsome (\a -> p (False # a))
    then False # epsilon (\a -> p (False # a))
    else True  # epsilon (\a -> p (True  # a))

forsome :: (Cantor -> Bool) -> Bool
forsome p = p (epsilon p)

forevery :: (Cantor -> Bool) -> Bool
forevery p = not (forsome (not . p))

-- Igualdad para funciones.
instance (Eq b) => Eq (Cantor -> b) where
  f == g = forevery (\u -> f u == g u)
