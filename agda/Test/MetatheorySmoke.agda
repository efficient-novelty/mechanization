{-# OPTIONS --cubical --safe --guardedness #-}

module Test.MetatheorySmoke where

open import PEN

affine-shift-available = U-is-fibonacci
affine-bootstrap-available = tau-bootstrap-closed
affine-depth1-available = tau-depth1-closed

extensional-available = history-truncates-to-one

upper-bound-available = history-beyond-two-algorithmically-subsumed
lower-bound-available = depth1-insufficient
