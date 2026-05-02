{-# OPTIONS --cubical --safe --guardedness #-}

module Test.SurfaceBridgeSmoke where

open import Metatheory.SurfaceNormalizationBridge
open import Metatheory.SurfaceToHornImage

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
surface-to-horn-normal-form-theorem = surface-to-horn-normal-form
surface-to-horn-support-theorem = surface-to-horn-preserves-support
surface-to-horn-arity-theorem = surface-to-horn-preserves-arity
surface-to-horn-primitive-cost-theorem =
  surface-to-horn-preserves-primitive-cost
higher-structural-derived-theorem = higher-structural-fields-derived
higher-raw-structural-derived-theorem =
  higher-raw-structural-traces-derived
no-naked-higher-structural-projections-theorem =
  raw-syntax-no-naked-higher-structural-projections
horn-image-completeness-theorem =
  horn-image-complete-for-structural-clauses
raw-structural-normalizes-to-horn-theorem =
  raw-structural-normalizes-to-horn
