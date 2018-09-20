-- https://www.cs.bham.ac.uk/~mhe/papers/omniscient-2011-07-06.pdf
{-# LANGUAGE FlexibleInstances #-}

-- CONATURAL.
-- Una representación de los números conaturales.
data Conat = Zero | Succ Conat deriving (Eq)

infinity :: Conat
infinity = Succ infinity

-- Hacerlos instancia de Num nos permitirá usar los enteros con
-- notación usual.
instance Num Conat where
  Zero + y = y
  Succ x + y = Succ (x + y)
  Zero * y = Zero
  Succ x * y = y + (x * y)
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n-1))

asInt :: Conat -> Integer
asInt Zero = 0
asInt (Succ n) = succ $ asInt n
instance Show Conat where
  show = show . asInt

-- Búsqueda usando las mismas funciones auxiliares que en el
-- caso de los números conaturales.
epsilon :: (Conat -> Bool) -> Conat
epsilon p = if p Zero
  then Zero
  else Succ $ epsilon (p . Succ)

forsome :: (Conat -> Bool) -> Bool
forsome p = p (epsilon p)

forevery :: (Conat -> Bool) -> Bool
forevery p = not (forsome (not . p))
