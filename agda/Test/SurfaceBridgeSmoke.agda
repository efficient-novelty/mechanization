{-# OPTIONS --cubical --safe --guardedness #-}

module Test.SurfaceBridgeSmoke where

open import Metatheory.SurfaceNormalizationBridge

canonical-normalized-signature-surface = CanonicalNormalizedSignature
raw-extension-candidate-surface = RawExtensionCandidate
raw-extension-candidate-theorem = raw-extension-elaborates-to-candidate
raw-extension-normalization-theorem =
  raw-extension-normalizes-to-canonical-signature
raw-trace-normalization-theorem =
  raw-trace-normalizes-to-canonical-signature
normalization-support-theorem = normalize-preserves-support
normalization-arity-theorem = normalize-preserves-arity
normalization-primitive-cost-theorem = normalize-preserves-primitive-cost
normalization-presentation-equivalence-theorem =
  normalization-respects-presentation-equivalence
normalization-counted-interface-theorem =
  normalized-signature-matches-counted-interface
