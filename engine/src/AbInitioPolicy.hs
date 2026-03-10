module AbInitioPolicy
  ( StrictAdmissibility(..)
  , InterfaceDebt(..)
  , strictAdmissibility
  , strictStepBudgetSeconds
  , strictInterfaceDebt
  , countActiveExports
  , estimateObligationSurface
  , capFromInterfaceDebt
  , bandsFromInterfaceDebt
  ) where

import Types (Library, LibraryEntry(..))

data StrictAdmissibility = StrictAdmissibility
  { saCap :: !Int
  , saBands :: ![Int]
  } deriving (Show, Eq)

data InterfaceDebt = InterfaceDebt
  { idActiveInterfaceSize :: !Int
  , idObligationSurface :: !Int
  , idSuggestedCap :: !Int
  , idSuggestedBands :: ![Int]
  } deriving (Show, Eq)

strictStepBudgetSeconds :: Int -> Int
strictStepBudgetSeconds step =
  case drop (max 0 (step - 1)) strictStepBudgets of
    (seconds:_) -> seconds
    [] -> last strictStepBudgets

strictStepBudgets :: [Int]
strictStepBudgets = [10, 15, 20, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195]

strictAdmissibility :: Int -> Library -> StrictAdmissibility
strictAdmissibility step lib
  | step == 1 = StrictAdmissibility 2 [2]
  | step == 2 = StrictAdmissibility 1 [1]
  | step == 3 = StrictAdmissibility 1 [1]
  | otherwise =
      let debt = strictInterfaceDebt 2 lib
      in StrictAdmissibility (capFromInterfaceDebt debt) (bandsFromInterfaceDebt debt)

strictInterfaceDebt :: Int -> Library -> InterfaceDebt
strictInterfaceDebt activeWindow lib =
  let activeSize = countActiveExports activeWindow lib
      obligationSurface = estimateObligationSurface activeWindow lib
      suggestedCap = debtToCap activeSize obligationSurface
      suggestedBands = debtToBands activeSize obligationSurface suggestedCap
  in InterfaceDebt
       { idActiveInterfaceSize = activeSize
       , idObligationSurface = obligationSurface
       , idSuggestedCap = suggestedCap
       , idSuggestedBands = suggestedBands
       }

countActiveExports :: Int -> Library -> Int
countActiveExports activeWindow lib =
  sum (map entryExportWeight (take (max 0 activeWindow) (reverse lib)))

estimateObligationSurface :: Int -> Library -> Int
estimateObligationSurface activeWindow lib =
  let active = take (max 0 activeWindow) (reverse lib)
      exportSurface = sum (map entryExportWeight active)
      pathSurface = sum [min 4 (length (lePathDims entry) + if leHasLoop entry then 1 else 0) | entry <- active]
      truncSurface = length [() | entry <- active, hasTruncEntry entry]
      interactionSurface = sum [flagSurface entry | entry <- active]
      crossCouplingSurface =
        sum
          [ min (entryExportWeight left) (entryExportWeight right)
          | (ix, left) <- zip [0 :: Int ..] active
          , right <- drop (ix + 1) active
          , isPureShell left
          , isPureShell right
          ]
      higherArityShellSurface =
        sum
          [ max 0 (entryExportWeight entry - 5)
          | entry <- active
          , isPureShell entry
          ]
      binderDebt = if any leHasDependentFunctions lib then 0 else 2
      bridgeDebt = if length (filter richInterfaceEntry active) >= 2 then 1 else 0
  in exportSurface + pathSurface + truncSurface + interactionSurface + crossCouplingSurface + higherArityShellSurface + binderDebt + bridgeDebt

capFromInterfaceDebt :: InterfaceDebt -> Int
capFromInterfaceDebt = idSuggestedCap

bandsFromInterfaceDebt :: InterfaceDebt -> [Int]
bandsFromInterfaceDebt = idSuggestedBands

debtToCap :: Int -> Int -> Int
debtToCap activeSize obligationSurface =
  min 9 (max 3 (3 + obligationSurface `div` 5 + activeSize `div` 10))

debtToBands :: Int -> Int -> Int -> [Int]
debtToBands activeSize obligationSurface cap
  | cap <= 3 = [3]
  | obligationSurface <= activeSize + 2 = [cap]
  | otherwise = [max 3 (cap - 1) .. cap]

entryExportWeight :: LibraryEntry -> Int
entryExportWeight entry =
  if isPureShell entry
  then max 1 (leAxiomaticExports entry)
  else max 1 (structuralExportWeight entry)

flagSurface :: LibraryEntry -> Int
flagSurface entry =
  sum
    [ boolWeight (leHasDependentFunctions entry)
    , boolWeight (leHasModalOps entry)
    , boolWeight (leHasDifferentialOps entry)
    , boolWeight (leHasCurvature entry)
    , boolWeight (leHasMetric entry)
    , boolWeight (leHasHilbert entry)
    , boolWeight (leHasTemporalOps entry)
    ]

richInterfaceEntry :: LibraryEntry -> Bool
richInterfaceEntry entry =
  entryExportWeight entry >= 3

structuralExportWeight :: LibraryEntry -> Int
structuralExportWeight entry =
  leConstructors entry
    + length (lePathDims entry)
    + boolWeight (leHasLoop entry)
    + truncWeight entry

isPureShell :: LibraryEntry -> Bool
isPureShell entry =
  leConstructors entry == 0
    && null (lePathDims entry)
    && not (hasTruncEntry entry)

truncWeight :: LibraryEntry -> Int
truncWeight entry =
  case leIsTruncated entry of
    Just _ -> 1
    Nothing -> 0

hasTruncEntry :: LibraryEntry -> Bool
hasTruncEntry entry =
  case leIsTruncated entry of
    Just _ -> True
    Nothing -> False

boolWeight :: Bool -> Int
boolWeight True = 1
boolWeight False = 0
