{-# OPTIONS --cubical --safe --guardedness #-}

module Test.ClutchingSmoke where

open import Geometry.Clutching

circle-clutching-boundary-surface = CircleClutchingBoundary
clutching-family-available = clutching-family
hopf-meridian-datum-surface = HopfMeridianDatum
hopf-clutching-family-available = HopfClutchingFamily
hopf-clutching-total-space-available = HopfClutchingTotalSpace
hopf-binary-clutching-datum-available = hopf-binary-clutching-datum
hopf-binary-clutching-nontrivial-available =
  hopf-binary-clutching-nontrivial
clutching-horn-fiber-surface = ClutchingHornExtensionFiber
clutching-horn-contractible-available =
  clutching-horn-extension-fiber-contractible
clutching-theorem-surface = ClutchingFamilyTheorem
clutching-theorem-available = clutching-family-theorem
