\begin{code}
{-# OPTIONS --without-K #-}

module Snippets where
data Nat : Set where
  zero : Nat
  succ : Nat → Nat
{-# BUILTIN NATURAL Nat #-}
\end{code}

%<*induction>
\begin{code}
recursion : {ϕ : Nat → Set}
  → ϕ 0
  → ({k : Nat} → ϕ k → ϕ (succ k))
  → (n : Nat) → ϕ n
recursion {ϕ} z s zero = z
recursion {ϕ} z s (succ n) = s (recursion {ϕ} z s n)
\end{code}
%</induction>

%<*unit>
\begin{code}
-- Proposición verdadera.
record ⊤ : Set where
  constructor *

-- Proposición falsa.
data ⊥ : Set where

-- Conjunción.
record _∧_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

-- Disyunción.
data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B
  inr : B → A ∨ B
\end{code}
%</unit>

%<*conjunction>
\begin{code}
swap : {A B : Set} → A ∧ B → B ∧ A
swap (a , b) = b , a
\end{code}
%</conjunction>


%<*naturalsexistential>
\begin{code}
record Σ (A : Set) (B : A → Set) : Set where
  constructor _**_
  field
    fst : A
    snd : B fst
\end{code}
%</naturalsexistential>

%<*equality>
\begin{code}
data _≡_ {A : Set} (a : A) : A → Set where
  refl : a ≡ a

symm : {A : Set}{a b : A} → a ≡ b → b ≡ a
symm refl = refl

trans : {A : Set}{a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl q = q

ap : {A B : Set}{a b : A} → (f : A → B) → a ≡ b → f a ≡ f b
ap f refl = refl
\end{code}
%</equality>

\begin{code}
{-# BUILTIN EQUALITY _≡_ #-}
\end{code}

%<*suma>
\begin{code}
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

infixl 30 _+_
_+_ : ℕ → ℕ → ℕ
zero + b = b
succ a + b = succ (a + b)

+rzero : ∀ n → n + zero ≡ n
+rzero zero = refl
+rzero (succ n) = ap succ (+rzero n)

+rsucc : ∀ n m → n + succ m ≡ succ (n + m)
+rsucc zero m = refl
+rsucc (succ n) m = ap succ (+rsucc n m)

+comm : ∀ n m → n + m ≡ m + n
+comm zero m rewrite +rzero m = refl
+comm (succ n) m rewrite +rsucc m n = ap succ (+comm n m)
\end{code}
%</suma>
