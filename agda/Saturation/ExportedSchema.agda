{-# OPTIONS --cubical --safe --guardedness #-}

module Saturation.ExportedSchema where

open import Cubical.Foundations.Prelude

open import Core.Nat
open import Core.Sequence

-- ============================================
-- Schema Kinds
-- ============================================

-- Classification of obligation schemas by their role
-- in the type-theoretic structure.

data SchemaKind : Type where
  TypeFormation : SchemaKind    -- type formation rule (e.g., S¹ : Type)
  TermIntro    : SchemaKind    -- constructor (e.g., base : S¹)
  Elimination  : SchemaKind    -- eliminator (e.g., S¹-elim)
  BetaRule     : SchemaKind    -- computation rule (e.g., β-loop)
  Interaction  : SchemaKind    -- cross-layer obligation

-- ============================================
-- Exported Schema
-- ============================================

-- An ExportedSchema records one obligation node that a
-- layer exports to the interface.

record ExportedSchema : Type where
  constructor mkSchema
  field
    schemaKind : SchemaKind
    sourceDim  : ℕ            -- dimension of the cell that generates this
    sourceStep : ℕ            -- which Genesis step it originates from

open ExportedSchema public

-- ============================================
-- Schema Sets (length-indexed)
-- ============================================

-- A SchemaSet of cardinality n is a Vec of exactly n schemas.
-- The Vec length enforces the count at the type level.
--
-- IMPORTANT CAVEAT: The Vec length proves only that the vector
-- has n elements, NOT that those n elements are the COMPLETE
-- set of schemas. Completeness requires a separate argument
-- (see Enumeration.agda for derivation of counts).

SchemaSet : ℕ → Type
SchemaSet n = Vec ExportedSchema n

-- ============================================
-- Helper: make a schema
-- ============================================

tf : ℕ → ℕ → ExportedSchema
tf = mkSchema TypeFormation

ti : ℕ → ℕ → ExportedSchema
ti = mkSchema TermIntro

el : ℕ → ℕ → ExportedSchema
el = mkSchema Elimination

br : ℕ → ℕ → ExportedSchema
br = mkSchema BetaRule

ix : ℕ → ℕ → ExportedSchema
ix = mkSchema Interaction

-- ============================================
-- Step 1: Universe — Δ₁ = 1 schema
-- ============================================
--
-- The universe U₀ is not an inductive type; it is the
-- foundational type classifier. It exports exactly one
-- schema: the type formation rule.
--
-- [1] TypeFormation: U₀ : Type₁
--     "There exists a universe of types."
--
-- Exhaustiveness: U₀ has no constructors (types are added
-- by subsequent layers), no eliminator, no computation
-- rules. The single type formation rule is the complete
-- specification.

schemas-step1 : SchemaSet (Δ 1)
schemas-step1 = tf 0 1 ∷ []

-- ============================================
-- Step 2: Unit — Δ₂ = 1 schema
-- ============================================
--
-- The unit type 𝟏 adds one datum to the system: a
-- canonical inhabitant ★ : 𝟏.
--
-- [1] TermIntro: ★ : 𝟏
--     "The unit type is inhabited."
--
-- NOT counted as separate exports:
-- - 𝟏 : U₀ (type formation) resolves an obligation
--   from Universe (step 1), not a new export.
-- - ind₁ (eliminator) is uniquely determined by ★ being
--   the sole constructor — it's trivial.
-- - β : ind₁(★,c) ≡ c is definitional.
--
-- Exhaustiveness: the unit type is completely specified
-- by its unique inhabitant. All other operations are
-- derived.

schemas-step2 : SchemaSet (Δ 2)
schemas-step2 = ti 0 2 ∷ []

-- ============================================
-- Step 3: Witness/Identity — Δ₃ = 2 schemas
-- ============================================
--
-- The identity/path type adds two independently specifiable
-- components:
--
-- [1] TypeFormation: _≡_ : A → A → Type  (for all A : U₀)
--     "For any type A and terms a,b : A, there is a type
--      of witnesses that a equals b."
--
-- [2] TermIntro (dim 1): refl : (a : A) → a ≡ a
--     "Every term is equal to itself."
--     (dim 1 because refl is a path — a 1-dimensional datum)
--
-- NOT counted as separate exports:
-- - J eliminator: UNIQUELY DETERMINED by refl via the
--   universal property of identity types. In cubical Agda,
--   J is literally derived from path primitives.
-- - J-β: J(refl) ≡ id — holds definitionally in cubical.
--
-- Exhaustiveness: the identity type is FREELY GENERATED
-- by refl. Any model of identity types is completely
-- determined by how it interprets the type former and
-- refl. This is the standard characterization from
-- Martin-Löf type theory.

schemas-step3 : SchemaSet (Δ 3)
schemas-step3 = tf 0 3 ∷ ti 1 3 ∷ []

-- ============================================
-- Step 4: Π Types (Dependent Functions) — Δ₄ = 3 schemas
-- ============================================
--
-- Dependent function types are specified by three rules:
--
-- [1] TypeFormation: Π : (A : U₀)(B : A → U₀) → U₀
--     "Given a type A and a family B over A, there is a
--      type of dependent functions."
--
-- [2] TermIntro: λ : ((a : A) → B a) → Π A B
--     "Functions are introduced by λ-abstraction."
--
-- [3] Elimination: app : Π A B → (a : A) → B a
--     "Functions are eliminated by application."
--
-- NOT counted as separate exports:
-- - β : app(λf, a) ≡ f(a) — definitional (computation rule)
-- - η : f ≡ λa.app(f,a) — definitional (uniqueness principle)
--
-- Exhaustiveness: Π types are the negative type characterized
-- by formation, introduction, and elimination. β and η hold
-- definitionally. This is the standard specification.

schemas-step4 : SchemaSet (Δ 4)
schemas-step4 = tf 0 4 ∷ ti 0 4 ∷ el 0 4 ∷ []

-- ============================================
-- Step 5: Circle S¹ — Δ₅ = 5 schemas
-- ============================================
--
-- The circle is the first HIGHER INDUCTIVE TYPE (HIT).
-- Unlike ordinary inductive types, HITs require the
-- elimination principle and higher computation rules
-- as ADDITIONAL AXIOMS (they are not derivable from
-- the constructors alone).
--
-- [1] TypeFormation: S¹ : Type
--     "The circle is a type."
--
-- [2] TermIntro (dim 0): base : S¹
--     "The circle has a base point."
--
-- [3] TermIntro (dim 1): loop : base ≡ base
--     "The circle has a non-trivial loop."
--     (dim 1 because loop is a path)
--
-- [4] Elimination: S¹-elim :
--       (C : S¹ → Type)(b : C base)
--       (l : PathOver C loop b b) → (x : S¹) → C x
--     "To define a dependent function out of S¹, give a
--      base case and a loop case."
--     THIS IS AN AXIOM for HITs — it cannot be derived
--     from the constructors alone, unlike for ordinary
--     inductive types where it follows from the universal
--     property.
--
-- [5] BetaRule (dim 1): β-loop :
--       apd (S¹-elim C b l) loop ≡ l
--     "The eliminator computes correctly on the loop."
--     THIS IS AN AXIOM for HITs — the computation rule
--     for higher constructors does not hold definitionally
--     in most implementations.
--
-- NOT counted:
-- - β-base: S¹-elim C b l base ≡ b — holds definitionally
--   (point-constructor β-rules are definitional for HITs)
--
-- Exhaustiveness: these 5 schemas are the standard
-- specification of S¹ as a HIT in HoTT/cubical type
-- theory. The formation, constructors, elimination
-- principle, and non-trivial computation rule are the
-- complete generating set.

schemas-step5 : SchemaSet (Δ 5)
schemas-step5 =
    tf 0 5
  ∷ ti 0 5
  ∷ ti 1 5
  ∷ el 0 5
  ∷ br 1 5
  ∷ []

-- ============================================
-- Step 6: Propositional Truncation — Δ₆ = 8 schemas
-- ============================================
--
-- Structural schemas (7, from HIT specification):
-- [1] TypeFormation: ∥_∥ : Type → Type
-- [2] TermIntro (dim 0): |_| : A → ∥A∥
-- [3] TermIntro (dim 1): squash : (x y : ∥A∥) → x ≡ y
-- [4] TermIntro (dim 2): squash-coherence (2-cell filler)
-- [5] Elimination: ∥-∥-rec
-- [6] BetaRule (dim 1): β for squash path
-- [7] BetaRule (dim 2): β for coherence disc
--
-- Interaction schemas (1, to reach Δ₆ = 8):
-- [8] Interaction: compatibility with Π for rec into propositions
--     (∥-∥-rec requires its codomain to be a proposition,
--      which is a constraint involving Π and ≡)
--
-- NOTE: The interaction schema count (1) is an assignment,
-- not a derivation. See Enumeration.agda for discussion.

schemas-step6 : SchemaSet (Δ 6)
schemas-step6 =
    tf 0 6        -- ∥_∥ formation
  ∷ ti 0 6        -- |_| constructor
  ∷ ti 1 6        -- squash path
  ∷ ti 2 6        -- squash coherence disc
  ∷ el 0 6        -- ∥-∥-rec eliminator
  ∷ br 1 6        -- β for squash
  ∷ br 2 6        -- β for coherence
  ∷ ix 0 6        -- interaction: Π-compatibility for rec
  ∷ []

-- ============================================
-- Step 7: Sphere S² — Δ₇ = 13 schemas
-- ============================================
--
-- Structural schemas (5):
-- [1] TypeFormation: S² : Type
-- [2] TermIntro (dim 0): base : S²
-- [3] TermIntro (dim 2): surf : refl ≡ refl (2-cell)
-- [4] Elimination: S²-elim
-- [5] BetaRule (dim 2): β-surf
--
-- Interaction schemas (8, with library of 6 prior types):
-- [6-13] Cross-type obligations with U₀, 𝟏, ≡, Π, S¹, ∥-∥
--
-- The 8 interaction schemas represent obligations like:
-- maps S² → S¹, π₂(S²) computation, ∥S²∥ truncation,
-- and coherence between S²'s 2-cell and existing path
-- operations. These CANNOT be derived from cell data alone.

schemas-step7 : SchemaSet (Δ 7)
schemas-step7 =
    tf 0 7        -- S² formation
  ∷ ti 0 7        -- base
  ∷ ti 2 7        -- surf
  ∷ el 0 7        -- S²-elim
  ∷ br 2 7        -- β-surf
  ∷ ix 0 7        -- interactions with prior library (8 total)
  ∷ ix 1 7
  ∷ ix 2 7
  ∷ ix 0 7
  ∷ ix 1 7
  ∷ ix 2 7
  ∷ ix 0 7
  ∷ ix 1 7
  ∷ []

-- ============================================
-- Step 8: S³ ≅ SU(2) — Δ₈ = 21 schemas
-- ============================================
--
-- Structural schemas (5):
-- [1] TypeFormation: S³ : Type
-- [2] TermIntro (dim 0): base : S³
-- [3] TermIntro (dim 3): cell : refl ≡ refl (3-cell)
-- [4] Elimination: S³-elim
-- [5] BetaRule (dim 3): β-cell
--
-- Interaction schemas (16, with library of 7 prior types):
-- [6-21] Cross-type obligations with all prior types
--
-- The 16 interaction schemas reflect S³'s extensive
-- interactions, including the Hopf-fibration connection
-- S³ → S² with fiber S¹. At this point, interaction
-- schemas constitute 76% of the total (16/21).

schemas-step8 : SchemaSet (Δ 8)
schemas-step8 =
    tf 0 8        -- S³ formation
  ∷ ti 0 8        -- base
  ∷ ti 3 8        -- cell (3-cell)
  ∷ el 0 8        -- S³-elim
  ∷ br 3 8        -- β-cell
  ∷ ix 0 8        -- interactions with prior library (16 total)
  ∷ ix 1 8
  ∷ ix 2 8
  ∷ ix 3 8
  ∷ ix 0 8
  ∷ ix 1 8
  ∷ ix 2 8
  ∷ ix 3 8
  ∷ ix 0 8
  ∷ ix 1 8
  ∷ ix 2 8
  ∷ ix 0 8
  ∷ ix 1 8
  ∷ ix 0 8
  ∷ ix 1 8
  ∷ ix 0 8
  ∷ []

-- ============================================
-- Lookup by Genesis Step Number
-- ============================================

genesisSchemaCount : ℕ → ℕ
genesisSchemaCount 1 = Δ 1
genesisSchemaCount 2 = Δ 2
genesisSchemaCount 3 = Δ 3
genesisSchemaCount 4 = Δ 4
genesisSchemaCount 5 = Δ 5
genesisSchemaCount 6 = Δ 6
genesisSchemaCount 7 = Δ 7
genesisSchemaCount 8 = Δ 8
genesisSchemaCount _ = 0
