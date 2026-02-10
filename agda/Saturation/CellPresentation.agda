{-# OPTIONS --cubical --safe --guardedness #-}

module Saturation.CellPresentation where

open import Cubical.Foundations.Prelude

open import Core.Nat

-- ============================================
-- Cell Presentations for Genesis Steps
-- ============================================

-- A CellPresentation records the CW-complex structure
-- of a homotopy type: how many cells in each dimension.
-- This is the combinatorial data that drives obligation counts.

record CellPresentation : Type where
  field
    dim0Cells     : ℕ    -- 0-cells (points)
    dim1Cells     : ℕ    -- 1-cells (paths)
    dim2Cells     : ℕ    -- 2-cells (discs)
    dim3PlusCells : ℕ    -- 3+-cells (higher)

open CellPresentation public

-- Total number of cells
totalCells : CellPresentation → ℕ
totalCells cp = dim0Cells cp + dim1Cells cp + dim2Cells cp + dim3PlusCells cp

-- ============================================
-- Genesis Step 1: Universe U₀
-- ============================================
-- The universe type itself: one point (the type former)
cellPres-Universe : CellPresentation
cellPres-Universe = record
  { dim0Cells     = 1
  ; dim1Cells     = 0
  ; dim2Cells     = 0
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 2: Unit Type 𝟏
-- ============================================
-- One point (the constructor ★)
cellPres-Unit : CellPresentation
cellPres-Unit = record
  { dim0Cells     = 1
  ; dim1Cells     = 0
  ; dim2Cells     = 0
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 3: Witness / Identity (Id)
-- ============================================
-- One point + one path (reflexivity)
cellPres-Witness : CellPresentation
cellPres-Witness = record
  { dim0Cells     = 1
  ; dim1Cells     = 1
  ; dim2Cells     = 0
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 4: Π/Σ Types (Dependent Types)
-- ============================================
-- Π and Σ type formers plus application and pairing
cellPres-PiSigma : CellPresentation
cellPres-PiSigma = record
  { dim0Cells     = 2
  ; dim1Cells     = 1
  ; dim2Cells     = 0
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 5: Circle S¹
-- ============================================
-- Standard CW: one 0-cell (base) + one 1-cell (loop)
cellPres-Circle : CellPresentation
cellPres-Circle = record
  { dim0Cells     = 1
  ; dim1Cells     = 1
  ; dim2Cells     = 0
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 6: Propositional Truncation ∥-∥
-- ============================================
-- |_| constructor + squash path + higher coherence
cellPres-PropTrunc : CellPresentation
cellPres-PropTrunc = record
  { dim0Cells     = 1
  ; dim1Cells     = 1
  ; dim2Cells     = 1
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 7: Sphere S²
-- ============================================
-- One 0-cell (base) + one 2-cell (surf)
cellPres-S2 : CellPresentation
cellPres-S2 = record
  { dim0Cells     = 1
  ; dim1Cells     = 0
  ; dim2Cells     = 1
  ; dim3PlusCells = 0
  }

-- ============================================
-- Genesis Step 8: S³ ≅ SU(2)
-- ============================================
-- One 0-cell (base) + one 3-cell
cellPres-S3 : CellPresentation
cellPres-S3 = record
  { dim0Cells     = 1
  ; dim1Cells     = 0
  ; dim2Cells     = 0
  ; dim3PlusCells = 1
  }

-- ============================================
-- Lookup by Genesis Step Number
-- ============================================

genesisCellPres : ℕ → CellPresentation
genesisCellPres 1 = cellPres-Universe
genesisCellPres 2 = cellPres-Unit
genesisCellPres 3 = cellPres-Witness
genesisCellPres 4 = cellPres-PiSigma
genesisCellPres 5 = cellPres-Circle
genesisCellPres 6 = cellPres-PropTrunc
genesisCellPres 7 = cellPres-S2
genesisCellPres 8 = cellPres-S3
genesisCellPres _ = record  -- default: empty
  { dim0Cells     = 0
  ; dim1Cells     = 0
  ; dim2Cells     = 0
  ; dim3PlusCells = 0
  }
