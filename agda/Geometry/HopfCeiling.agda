{-# OPTIONS --cubical --guardedness #-}

module Geometry.HopfCeiling where

open import Cubical.Foundations.Prelude
open import Cubical.Data.Sigma.Base using (Σ; _,_)

-- Artifact B: principal-bundle cocycle checking under strict associativity,
-- and explicit coherence payload for non-associative composition.

record AssocOp : Type₁ where
  field
    Carrier : Type
    _⋆_     : Carrier → Carrier → Carrier
    assoc   : (x y z : Carrier) → (_⋆_ (_⋆_ x y) z) ≡ (_⋆_ x (_⋆_ y z))

open AssocOp public

mul : (A : AssocOp) → Carrier A → Carrier A → Carrier A
mul A = AssocOp._⋆_ A

record TripleData (A : AssocOp) : Type where
  field
    g01 g12 g23 g02 g13 g03 : Carrier A

open TripleData public

record Cocycle (A : AssocOp) (t : TripleData A) : Type where
  field
    c012 : mul A (g01 t) (g12 t) ≡ g02 t
    c123 : mul A (g12 t) (g23 t) ≡ g13 t
    c013 : mul A (g01 t) (g13 t) ≡ g03 t

open Cocycle public

reassociate
  : (A : AssocOp)
  → (x y z : Carrier A)
  → mul A (mul A x y) z ≡ mul A x (mul A y z)
reassociate A = assoc A

strict-4fold-consistency
  : (A : AssocOp)
  → (t : TripleData A)
  → (C : Cocycle A t)
  → Σ (mul A (mul A (g01 t) (g12 t)) (g23 t) ≡ mul A (g01 t) (mul A (g12 t) (g23 t)))
      (λ _ → mul A (mul A (g01 t) (g12 t)) (g23 t) ≡ g03 t)
strict-4fold-consistency A t C =
  let a₁ : mul A (mul A (g01 t) (g12 t)) (g23 t) ≡ mul A (g01 t) (mul A (g12 t) (g23 t))
      a₁ = assoc A (g01 t) (g12 t) (g23 t)

      a₂ : mul A (g01 t) (mul A (g12 t) (g23 t)) ≡ mul A (g01 t) (g13 t)
      a₂ = cong (λ z → mul A (g01 t) z) (c123 C)

      a₃ : mul A (g01 t) (g13 t) ≡ g03 t
      a₃ = c013 C
  in a₁ , (a₁ ∙ a₂ ∙ a₃)

-- Non-associative interface (octonionic-style): composition exists, but strict
-- reassociation is not built in and must be supplied as additional coherence.
record NonAssocOp : Type₁ where
  field
    Carrier : Type
    _⊛_     : Carrier → Carrier → Carrier

open NonAssocOp public

nmul : (A : NonAssocOp) → Carrier A → Carrier A → Carrier A
nmul A = NonAssocOp._⊛_ A

record WeakTripleData (A : NonAssocOp) : Type where
  field
    g01 g12 g23 g02 g13 g03 : Carrier A

open WeakTripleData public

record WeakCocycle (A : NonAssocOp) (t : WeakTripleData A) : Type where
  field
    c012 : nmul A (g01 t) (g12 t) ≡ g02 t
    c123 : nmul A (g12 t) (g23 t) ≡ g13 t
    c013 : nmul A (g01 t) (g13 t) ≡ g03 t

open WeakCocycle public

-- Extra coherence payload required to compare parenthesizations.
record CoherencePayload (A : NonAssocOp) : Type where
  field
    reassoc : (x y z : Carrier A) → nmul A (nmul A x y) z ≡ nmul A x (nmul A y z)

open CoherencePayload public

weak-4fold-consistency
  : (A : NonAssocOp)
  → (P : CoherencePayload A)
  → (t : WeakTripleData A)
  → (C : WeakCocycle A t)
  → Σ (nmul A (nmul A (g01 t) (g12 t)) (g23 t) ≡ nmul A (g01 t) (nmul A (g12 t) (g23 t)))
      (λ _ → nmul A (nmul A (g01 t) (g12 t)) (g23 t) ≡ g03 t)
weak-4fold-consistency A P t C =
  let b₁ : nmul A (nmul A (g01 t) (g12 t)) (g23 t) ≡ nmul A (g01 t) (nmul A (g12 t) (g23 t))
      b₁ = reassoc P (g01 t) (g12 t) (g23 t)

      b₂ : nmul A (g01 t) (nmul A (g12 t) (g23 t)) ≡ nmul A (g01 t) (g13 t)
      b₂ = cong (λ z → nmul A (g01 t) z) (c123 C)

      b₃ : nmul A (g01 t) (g13 t) ≡ g03 t
      b₃ = c013 C
  in b₁ , (b₁ ∙ b₂ ∙ b₃)
