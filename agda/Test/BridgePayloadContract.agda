{-# OPTIONS --guardedness --without-K #-}

module Test.BridgePayloadContract where

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List)
open import Agda.Builtin.Unit using (⊤ ; tt)

-- ============================================
-- Phase 6 Bridge Payload Contract Harness
-- ============================================
--
-- This module is the Agda-side schema/proof-obligation harness for bridge
-- payload JSON exported by `agda-bridge` (P6-WP2). The payload record mirrors
-- required fields so bridge claims can be consumed by Agda-side checks.
--
-- Note: JSON decoding is intentionally handled outside this module; this file
-- defines the contract and proof-obligation skeletons that imported payloads
-- must satisfy.
-- ============================================

record NuClaim : Set where
  constructor mkNuClaim
  field
    nu-g     : Nat
    nu-h     : Nat
    nu-c     : Nat
    nu-total : Nat

record BridgePayload : Set where
  constructor mkBridgePayload
  field
    step            : Nat
    name            : String
    canonical-key   : String
    kappa-bit       : Nat
    kappa-desugared : Nat
    anonymous-ast   : List String
    nu-claim        : NuClaim

-- ============================================
-- Contract obligations for independent verification
-- ============================================

postulate
  -- Canonicalization contract: the canonical key in the payload agrees with
  -- the canonical-key function Agda-side checks apply to anonymous AST entries.
  CanonicalKeySound : BridgePayload → Set

  -- ν decomposition contract: exported ν claim satisfies decomposition
  -- assumptions consumed by Agda-side novelty checks.
  NuClaimWellFormed : BridgePayload → Set

  -- Decode/reporting non-interference contract: bridge payload metadata may be
  -- consumed for interpretation, but must not alter selected anonymous AST.
  DecodeNonInterference : BridgePayload → Set

-- A minimal witness bundle used by CI harness to ensure the contract surface
-- remains type-correct as modules evolve.
record ContractWitness (p : BridgePayload) : Set where
  constructor mkWitness
  field
    canonical-soundness  : CanonicalKeySound p
    nu-claim-well-formed : NuClaimWellFormed p
    decode-noninterf     : DecodeNonInterference p

-- Placeholder driver marker for bridge-harness checks.
bridge-payload-contract-ready : ⊤
bridge-payload-contract-ready = tt
