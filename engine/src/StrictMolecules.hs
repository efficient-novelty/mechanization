module StrictMolecules
  ( PrimitiveNode(..)
  , APISkeleton(..)
  , ClauseProvenance(..)
  , MolecularPackage(..)
  , CompiledMolecule(..)
  , enumerateStrictMolecularCandidates
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (Telescope(..), TeleEntry(..))
import Types (Library, LibraryEntry(..))

import Data.List (sortOn)

data PrimitiveNode
  = PrimPi
  | PrimSigma
  | PrimTrunc
  deriving (Show, Eq, Ord)

data APISkeleton
  = MapBridgeShell
  | ModalShell
  | ConnectionShell
  | CurvatureShell
  | MetricShell
  | HilbertShell
  | TemporalShell
  deriving (Show, Eq, Ord)

data MolecularPackage
  = ActivateAmbient PrimitiveNode
  | DefineHIT Int Int
  | DefineAPI APISkeleton
  deriving (Show, Eq, Ord)

data ClauseProvenance
  = AxiomaticSignature
  | FrameworkDerived
  deriving (Show, Eq, Ord)

data CompiledMolecule = CompiledMolecule
  { cmPackages :: ![MolecularPackage]
  , cmConceptualKappa :: !Int
  , cmAxiomaticExports :: !Int
  , cmCompiledTelescope :: !Telescope
  , cmClauseProvenance :: ![ClauseProvenance]
  } deriving (Show, Eq)

enumerateStrictMolecularCandidates :: Library -> Int -> [CompiledMolecule]
enumerateStrictMolecularCandidates lib exactBand =
  [ let compiled = compileBundle lib bundle
    in CompiledMolecule
         bundle
         exactBand
         (bundleExportArity bundle)
         (Telescope (map fst compiled))
         (map snd compiled)
  | bundle <- generateBundles exactBand (availablePackages lib)
  ]

availablePackages :: Library -> [MolecularPackage]
availablePackages lib =
  concat
    [ if not hasDependentFunctions
      then [ActivateAmbient PrimPi, ActivateAmbient PrimSigma]
      else []
    , if hasDependentFunctions
      then [DefineHIT 1 1]
      else []
    , if hasLoop && not hasTruncation
      then [ActivateAmbient PrimTrunc]
      else []
    , if maxPathDim >= 1
      then [DefineHIT 1 2]
      else []
    , if maxPathDim >= 2
      then [DefineHIT 1 3]
      else []
    , if maxPathDim >= 3 && hasExactMaxDim 2 && hasExactMaxDim 1
      then [DefineAPI MapBridgeShell]
      else []
    , if hasMapBridge && not hasModalOps
      then [DefineAPI ModalShell]
      else []
    , if hasModalOps && not hasDifferentialOps
      then [DefineAPI ConnectionShell]
      else []
    , if hasDifferentialOps && not hasCurvature
      then [DefineAPI CurvatureShell]
      else []
    , if hasDifferentialOps && hasCurvature && not hasMetric
      then [DefineAPI MetricShell]
      else []
    , if hasDifferentialOps && hasCurvature && hasMetric && not hasHilbert
      then [DefineAPI HilbertShell]
      else []
    , if hasModalOps && hasHilbert && not hasTemporalOps
      then [DefineAPI TemporalShell]
      else []
    ]
  where
    hasDependentFunctions = any leHasDependentFunctions lib
    hasLoop = any leHasLoop lib
    hasMapBridge =
      any
        (\entry -> leHasLoop entry && null (lePathDims entry) && leHasDependentFunctions entry)
        lib
    hasModalOps = any leHasModalOps lib
    hasDifferentialOps = any leHasDifferentialOps lib
    hasCurvature = any leHasCurvature lib
    hasMetric = any leHasMetric lib
    hasHilbert = any leHasHilbert lib
    hasTemporalOps = any leHasTemporalOps lib
    hasTruncation =
      any
        (\entry -> case leIsTruncated entry of
          Just _ -> True
          Nothing -> False)
        lib
    maxPathDim = maximum (0 : concatMap lePathDims lib)
    hasExactMaxDim d =
      any
        (\entry -> not (null (lePathDims entry)) && maximum (lePathDims entry) == d)
        lib

generateBundles :: Int -> [MolecularPackage] -> [[MolecularPackage]]
generateBundles target packages =
  go target (sortOn packageOrder packages)
  where
    go remaining _
      | remaining < 0 = []
    go 0 _ = [[]]
    go _ [] = []
    go remaining (pkg:rest) =
      let withoutPkg = go remaining rest
          withPkg
            | cost > remaining = []
            | otherwise = map (pkg :) (go (remaining - cost) rest)
          cost = packageConceptualKappa pkg
      in withPkg ++ withoutPkg

packageConceptualKappa :: MolecularPackage -> Int
packageConceptualKappa pkg = case pkg of
  ActivateAmbient PrimPi -> 2
  ActivateAmbient PrimSigma -> 1
  ActivateAmbient PrimTrunc -> 3
  DefineHIT 1 1 -> 3
  DefineHIT 1 2 -> 3
  DefineHIT 1 3 -> 5
  DefineHIT points paths -> points + 2 * paths
  DefineAPI MapBridgeShell -> 4
  DefineAPI ModalShell -> 4
  DefineAPI ConnectionShell -> 5
  DefineAPI CurvatureShell -> 6
  DefineAPI MetricShell -> 7
  DefineAPI HilbertShell -> 9
  DefineAPI TemporalShell -> 8

packageExportArity :: MolecularPackage -> Int
packageExportArity pkg = case pkg of
  ActivateAmbient PrimPi -> 1
  ActivateAmbient PrimSigma -> 1
  ActivateAmbient PrimTrunc -> 3
  DefineHIT points paths -> 1 + points + paths
  DefineAPI MapBridgeShell -> 4
  DefineAPI ModalShell -> 4
  DefineAPI ConnectionShell -> 5
  DefineAPI CurvatureShell -> 6
  DefineAPI MetricShell -> 7
  DefineAPI HilbertShell -> 9
  DefineAPI TemporalShell -> 8

bundleExportArity :: [MolecularPackage] -> Int
bundleExportArity =
  max 1 . sum . map packageExportArity

compileBundle :: Library -> [MolecularPackage] -> [(TeleEntry, ClauseProvenance)]
compileBundle lib packages
  | hasPi && hasSigma =
      [ (TeleEntry "lam" (Lam (Pi (Var 1) (Var 2))), AxiomaticSignature)
      , (TeleEntry "pair" (App (App (Var 1) (Var 2)) (Var 3)), AxiomaticSignature)
      , (TeleEntry "app" (App (Lam (Var 1)) (Var 2)), AxiomaticSignature)
      ]
  | otherwise =
      concatMap (compilePackage lib) ordered
  where
    ordered = sortOn packageOrder packages
    hasPi = ActivateAmbient PrimPi `elem` packages
    hasSigma = ActivateAmbient PrimSigma `elem` packages

compilePackage :: Library -> MolecularPackage -> [(TeleEntry, ClauseProvenance)]
compilePackage lib pkg = case pkg of
  ActivateAmbient PrimPi ->
    [ (TeleEntry "lam" (Lam (Pi (Var 1) (Var 2))), AxiomaticSignature)
    , (TeleEntry "app" (App (Lam (Var 1)) (Var 2)), FrameworkDerived)
    ]
  ActivateAmbient PrimSigma ->
    [ (TeleEntry "pair" (App (App (Var 1) (Var 2)) (Var 3)), AxiomaticSignature)
    ]
  ActivateAmbient PrimTrunc ->
    [ (TeleEntry "trunc-form" (Trunc (Var 1)), AxiomaticSignature)
    , (TeleEntry "trunc-intro" (App (Trunc (Var 1)) (Var 2)), AxiomaticSignature)
    , (TeleEntry "squash" (PathCon 1), AxiomaticSignature)
    ]
    ++ derivedElaboratorClauses "trunc" [1]
  DefineHIT 1 1 ->
    [ (TeleEntry "hit-form" (App Univ (Var 1)), AxiomaticSignature)
    , (TeleEntry "base" (Var 1), AxiomaticSignature)
    , (TeleEntry "loop" (PathCon 1), AxiomaticSignature)
    ]
    ++ derivedElaboratorClauses "hit" [1]
  DefineHIT 1 2 ->
    [ (TeleEntry "hit-form" (App Univ (Var 1)), AxiomaticSignature)
    , (TeleEntry "base" (Var 1), AxiomaticSignature)
    , (TeleEntry "surf" (PathCon 2), AxiomaticSignature)
    ]
    ++ derivedElaboratorClauses "hit" [2]
  DefineHIT 1 3 ->
    [ (TeleEntry "hit-form" (App Univ (Var 1)), AxiomaticSignature)
    , (TeleEntry "base" (Var 1), AxiomaticSignature)
    , (TeleEntry "surf" (PathCon 3), AxiomaticSignature)
    , (TeleEntry "fill-n" (Lam (Var 1)), AxiomaticSignature)
    , (TeleEntry "fill-s" (Lam (Var 2)), AxiomaticSignature)
    ]
    ++ derivedElaboratorClauses "hit" [3]
  DefineHIT points paths ->
    (TeleEntry "hit-form" (App Univ (Var 1)), AxiomaticSignature)
      : [ (TeleEntry ("point" ++ show ix) (Var 1), AxiomaticSignature)
        | ix <- [1 .. points]
        ]
        ++ [ (TeleEntry ("path" ++ show ix) (PathCon 1), AxiomaticSignature)
           | ix <- [1 .. paths]
           ]
  DefineAPI MapBridgeShell ->
    [ (TeleEntry "hopf-map" (Pi (Lib srcRef) (Lib tgtRef)), AxiomaticSignature)
    , (TeleEntry "hopf-fiber" (Sigma (Lib tgtRef) (Lib fiberRef)), AxiomaticSignature)
    , (TeleEntry "hopf-total" (Lam (App (Lib srcRef) (Lib tgtRef))), AxiomaticSignature)
    , (TeleEntry "hopf-coh" (Id (Lib tgtRef) (Lib tgtRef) (Lib tgtRef)), AxiomaticSignature)
    ]
  DefineAPI ModalShell ->
    case modalCarrierRef of
      Nothing -> fallbackShell
      Just carrierRef ->
        [ (TeleEntry "flat" (Flat (Lib carrierRef)), AxiomaticSignature)
        , (TeleEntry "sharp" (Sharp (Lib carrierRef)), AxiomaticSignature)
        , (TeleEntry "disc" (Disc (Lib carrierRef)), AxiomaticSignature)
        , (TeleEntry "shape" (Shape (Lib carrierRef)), AxiomaticSignature)
        ]
  DefineAPI ConnectionShell ->
    case modalCapRef of
      Nothing -> fallbackShell
      Just modalRef ->
        [ (TeleEntry "conn-form" (Pi (Lib modalRef) (Pi (Var 1) (Var 1))), AxiomaticSignature)
        , (TeleEntry "cov" (Pi (Var 1) (Lam (Pi (Var 1) (Var 2)))), AxiomaticSignature)
        , (TeleEntry "transport" (Pi (Var 2) (Pi (Flat (Var 1)) (Var 1))), AxiomaticSignature)
        , (TeleEntry "modal-act" (App (Var 3) (App (Lib modalRef) (Var 1))), AxiomaticSignature)
        , (TeleEntry "cov-unit" (Pi (Var 4) (Lam (Var 1))), AxiomaticSignature)
        ]
  DefineAPI CurvatureShell ->
    case differentialCapRef of
      Nothing -> fallbackShell
      Just differentialRef ->
        [ (TeleEntry "curv-form" (Pi (Lib differentialRef) (Pi (Var 1) (Var 1))), AxiomaticSignature)
        , (TeleEntry "holonomy" (Pi (Var 1) (Lam (App (Lib differentialRef) (Var 1)))), AxiomaticSignature)
        , (TeleEntry "chern" (Pi (Var 2) (Lib differentialRef)), AxiomaticSignature)
        , (TeleEntry "curv-comp" (App (Var 3) (App (Lib differentialRef) (App (Var 1) (Var 2)))), AxiomaticSignature)
        , (TeleEntry "transport-law" (Pi (Var 4) (Lam (Pi (Var 1) (Var 2)))), AxiomaticSignature)
        , (TeleEntry "curv-bundle" (Pi (Var 5) (Lib differentialRef)), AxiomaticSignature)
        ]
  DefineAPI MetricShell ->
    case (differentialCapRef, curvatureCapRef) of
      (Just differentialRef, Just curvatureRef) ->
        [ (TeleEntry "metric" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1))), AxiomaticSignature)
        , (TeleEntry "lc" (Pi (Var 1) (Pi (Sigma (Var 1) (Var 2)) (Lib differentialRef))), AxiomaticSignature)
        , (TeleEntry "hodge" (Pi (Var 2) (Pi (Var 1) (Var 1))), AxiomaticSignature)
        , (TeleEntry "laplace" (Lam (App (Var 4) (App (Var 1) (Var 2)))), AxiomaticSignature)
        , (TeleEntry "ricci" (Pi (Var 4) (Lib curvatureRef)), AxiomaticSignature)
        , (TeleEntry "vol" (Pi (Var 5) (Lam (Pi (Var 1) (Var 1)))), AxiomaticSignature)
        , (TeleEntry "scalar" (Pi (Var 6) (Pi (Lib curvatureRef) (Var 1))), AxiomaticSignature)
        ]
      _ -> fallbackShell
  DefineAPI HilbertShell ->
    case (differentialCapRef, curvatureCapRef, metricCapRef) of
      (Just differentialRef, Just curvatureRef, Just metricRef) ->
        [ (TeleEntry "inner" (Sigma (Pi (Var 1) (Pi (Var 1) Univ)) (Var 1)), AxiomaticSignature)
        , (TeleEntry "complete" (Pi (Var 1) (Var 1)), AxiomaticSignature)
        , (TeleEntry "spectral" (Pi (Var 2) (Sigma (Var 1) (Var 1))), AxiomaticSignature)
        , (TeleEntry "functional" (Pi (Var 3) (Sigma (Lam (Var 1)) (Sigma (Var 1) (Var 2)))), AxiomaticSignature)
        , (TeleEntry "orthogonal" (Pi (Var 4) (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))), AxiomaticSignature)
        , (TeleEntry "metric-op" (Pi (Var 5) (Lib metricRef)), AxiomaticSignature)
        , (TeleEntry "curv-op" (Pi (Var 6) (Lib curvatureRef)), AxiomaticSignature)
        , (TeleEntry "conn-op" (Pi (Var 7) (Lib differentialRef)), AxiomaticSignature)
        , (TeleEntry "frechet" (Pi (Var 8) (Lam (Pi (Var 1) Univ))), AxiomaticSignature)
        ]
      _ -> fallbackShell
  DefineAPI TemporalShell ->
    case modalCapRef of
      Nothing -> fallbackShell
      Just modalRef ->
        [ (TeleEntry "next" (Next (Var 1)), AxiomaticSignature)
        , (TeleEntry "eventually" (Eventually (Var 1)), AxiomaticSignature)
        , (TeleEntry "flow" (Pi (Next (Var 1)) (Eventually (Var 1))), AxiomaticSignature)
        , (TeleEntry "guard" (Lam (App (Lib modalRef) (Next (Var 1)))), AxiomaticSignature)
        , (TeleEntry "flat-next" (Pi (Flat (Next (Var 1))) (Next (Flat (Var 1)))), AxiomaticSignature)
        , (TeleEntry "sharp-eventually" (Pi (Sharp (Eventually (Var 1))) (Eventually (Sharp (Var 1)))), AxiomaticSignature)
        , (TeleEntry "ev-app" (Lam (App (Eventually (Var 1)) (Var 2))), AxiomaticSignature)
        , (TeleEntry "next-mul" (Pi (Next (Next (Var 1))) (Next (Var 1))), AxiomaticSignature)
        ]
  where
    fallbackShell =
      [ (TeleEntry "api-shell" (Pi (Var 1) (Var 1)), AxiomaticSignature)
      ]
    refsByDim d = [i | (i, e) <- zip [1..] lib, d `elem` lePathDims e]
    refsByExactMaxDim d =
      [ i
      | (i, e) <- zip [1..] lib
      , not (null (lePathDims e))
      , maximum (lePathDims e) == d
      ]
    loopRefsByExactMaxDim d =
      [ i
      | (i, e) <- zip [1..] lib
      , leHasLoop e
      , not (null (lePathDims e))
      , maximum (lePathDims e) == d
      ]
    srcRef = latestPreferred [refsByExactMaxDim 3, refsByDim 3]
    tgtRef = latestPreferred [refsByExactMaxDim 2, refsByDim 2]
    fiberRef = latestPreferred [loopRefsByExactMaxDim 1, refsByExactMaxDim 1, refsByDim 1]
    modalCarrierRef = preferredModalCarrierRef lib
    modalCapRef = latestCapabilityRef leHasModalOps lib
    differentialCapRef = latestCapabilityRef leHasDifferentialOps lib
    curvatureCapRef = latestCapabilityRef leHasCurvature lib
    metricCapRef = latestCapabilityRef leHasMetric lib

    latestPreferred candidates =
      case dropWhile null candidates of
        (refs:_) ->
          case reverse refs of
            (i:_) -> i
            [] -> max 1 (length lib)
        [] -> max 1 (length lib)

latestCapabilityRef :: (LibraryEntry -> Bool) -> Library -> Maybe Int
latestCapabilityRef hasCap lib =
  case reverse [i | (i, e) <- zip [1..] lib, hasCap e] of
    (i:_) -> Just i
    [] -> Nothing

preferredModalCarrierRef :: Library -> Maybe Int
preferredModalCarrierRef lib =
  case reverse richLoopRefs of
    (i:_) -> Just i
    [] ->
      case reverse loopRefs of
        (i:_) -> Just i
        [] -> Nothing
  where
    indexed = zip [1..] lib
    loopRefs =
      [ i
      | (i, e) <- indexed
      , leHasLoop e
      ]
    richLoopRefs =
      [ i
      | (i, e) <- indexed
      , leHasLoop e
      , leConstructors e > 0
      , case leIsTruncated e of
          Nothing -> True
          Just _ -> False
      ]

packageOrder :: MolecularPackage -> Int
packageOrder pkg = case pkg of
  ActivateAmbient PrimPi -> 10
  ActivateAmbient PrimSigma -> 20
  ActivateAmbient PrimTrunc -> 30
  DefineHIT _ _ -> 40
  DefineAPI _ -> 50

derivedElaboratorClauses :: String -> [Int] -> [(TeleEntry, ClauseProvenance)]
derivedElaboratorClauses stem pathDims =
  [ (TeleEntry (stem ++ "-elim") (Lam (App (Var 1) (Var 2))), FrameworkDerived)
  , (TeleEntry (stem ++ "-beta-point") (Id (Var 1) (App (Var 1) (Var 1)) (Var 1)), FrameworkDerived)
  ]
    ++
    [ ( TeleEntry
          (stem ++ "-beta-path" ++ show dim)
          (Id (Var 1) (App (Var 1) (PathCon dim)) (App (Var 1) (PathCon dim)))
      , FrameworkDerived
      )
    | dim <- pathDims
    ]
