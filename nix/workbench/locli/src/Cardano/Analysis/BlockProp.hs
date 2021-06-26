{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}
module Cardano.Analysis.BlockProp (module Cardano.Analysis.BlockProp) where

import           Prelude (String, (!!), error, head, id, show, tail)
import           Cardano.Prelude hiding (head, show)

import           Control.Arrow ((***), (&&&))
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as AE
import           Data.Bifunctor
import           Data.Function (on)
import           Data.List (dropWhileEnd, intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map

import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import           Text.Printf (printf)

import           Ouroboros.Network.Block (BlockNo(..), SlotNo(..))

import           Data.Accum
import           Data.Distribution
import           Cardano.Profile
import           Cardano.Unlog.LogObject hiding (Text)
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats

import qualified Debug.Trace as D


data BlockPropagation
  = BlockPropagation
    { bpForgerForges        :: !(Distribution Float NominalDiffTime)
    , bpForgerAdoptions     :: !(Distribution Float NominalDiffTime)
    , bpForgerAnnouncements :: !(Distribution Float NominalDiffTime)
    , bpForgerSends         :: !(Distribution Float NominalDiffTime)
    , bpPeerNotices         :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerRequests        :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerFetches         :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerAdoptions       :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerAnnouncements   :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerSends           :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpChainBlockEvents    :: [BlockEvents]
    }
  deriving Show

instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "forged"        (f!!0) "Forge"   $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"   $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!2) "Announ"  $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!3) "Sendin"  $ DDeltaT bpForgerSends
    , Field 4 1 "noticedVal"    (p!!0) " Noti"   $ DDeltaT (fst . bpPeerNotices)
    , Field 4 0 "noticedCoV"    (p!!1) "ced  "   $ DDeltaT (snd . bpPeerNotices)
    , Field 4 1 "requestedVal"  (p!!2) "Reque"   $ DDeltaT (fst . bpPeerRequests)
    , Field 4 0 "requestedVal"  (p!!3) "sted "   $ DDeltaT (snd . bpPeerRequests)
    , Field 4 1 "fetchedVal"    (p!!4) " Fetc"   $ DDeltaT (fst . bpPeerFetches)
    , Field 4 0 "fetchedCoV"    (p!!5) "hed  "   $ DDeltaT (snd . bpPeerFetches)
    , Field 4 1 "pAdoptedVal"   (p!!6) " Adop"   $ DDeltaT (fst . bpPeerAdoptions)
    , Field 4 0 "pAdoptedCoV"   (p!!7) "ted  "   $ DDeltaT (snd . bpPeerAdoptions)
    , Field 4 1 "pAnnouncedVal" (p!!8) "Annou"  $ DDeltaT (fst . bpPeerAnnouncements)
    , Field 4 0 "pAnnouncedCoV" (p!!9) "nced "  $ DDeltaT (snd . bpPeerAnnouncements)
    , Field 4 1 "pSendStartVal" (p!!10) " Send" $ DDeltaT (fst . bpPeerSends)
    , Field 4 0 "pSendStartCoV" (p!!11) "ing  " $ DDeltaT (snd . bpPeerSends)
    ]
   where
     f = nChunksEachOf  4 7 "Forger event Δt:"
     p = nChunksEachOf 12 5 "Peer event Δt, and coefficients of variation:"

instance AE.ToJSON BlockPropagation where
  toJSON BlockPropagation{..} = AE.Array $ Vec.fromList
    [ extendObject "kind" "forgerForges"        $ toJSON bpForgerForges
    , extendObject "kind" "forgerAdoptions"     $ toJSON bpForgerAdoptions
    , extendObject "kind" "forgerAnnouncements" $ toJSON bpForgerAnnouncements
    , extendObject "kind" "forgerSends"         $ toJSON bpForgerSends
    , extendObject "kind" "peerNoticesMean"       $ toJSON (fst bpPeerNotices)
    , extendObject "kind" "peerNoticesCoV"        $ toJSON (snd bpPeerNotices)
    , extendObject "kind" "peerRequestsMean"      $ toJSON (fst bpPeerRequests)
    , extendObject "kind" "peerRequestsCoV"       $ toJSON (snd bpPeerRequests)
    , extendObject "kind" "peerFetchesMean"       $ toJSON (fst bpPeerFetches)
    , extendObject "kind" "peerFetchesCoV"        $ toJSON (snd bpPeerFetches)
    , extendObject "kind" "peerAdoptionsMean"     $ toJSON (fst bpPeerAdoptions)
    , extendObject "kind" "peerAdoptionsCoV"      $ toJSON (snd bpPeerAdoptions)
    , extendObject "kind" "peerAnnouncementsMean" $ toJSON (fst bpPeerAnnouncements)
    , extendObject "kind" "peerAnnouncementsCoV"  $ toJSON (snd bpPeerAnnouncements)
    , extendObject "kind" "peerSendsMean"         $ toJSON (fst bpPeerSends)
    , extendObject "kind" "peerSendsCoV"          $ toJSON (snd bpPeerSends)
    ]

data BPError
  = BPError
  { eBlock :: !Hash
  , eFile  :: !(Maybe FilePath)
  , eLO    :: !(Maybe LogObject)
  , eDesc  :: !BPErrorKind
  }
  deriving (FromJSON, Generic, Show, ToJSON)

data BPErrorKind
  = BPEBefore                !Phase !Phase
  | BPEUnexpectedForObserver !Phase
  | BPEUnexpectedForForger   !Phase
  | BPEUnexpectedAsFirst     !Phase
  | BPEDuplicateForge
  | BPEMissingPhase          !Phase
  | BPENegativePhase         !Phase !NominalDiffTime
  | BPEFork                  !Hash
  deriving (FromJSON, Generic, Show, ToJSON)

bpeIsFork, bpeIsMissingAny, bpeIsNegativeAny  :: BPError -> Bool
bpeIsFork BPError{eDesc=BPEFork{}} = True
bpeIsFork _ = False
bpeIsMissingAny BPError{eDesc=BPEMissingPhase{}} = True
bpeIsMissingAny _ = False
bpeIsNegativeAny BPError{eDesc=BPENegativePhase{}} = True
bpeIsNegativeAny _ = False

bpeIsMissing, bpeIsNegative  :: Phase -> BPError -> Bool
bpeIsMissing  p BPError{eDesc=BPEMissingPhase p'} = p == p'
bpeIsMissing  _ _ = False
bpeIsNegative p BPError{eDesc=BPENegativePhase p' _} = p == p'
bpeIsNegative _ _ = False

data Phase
  = Notice
  | Request
  | Fetch
  | Forge
  | Acquire
  | Adopt
  | Announce
  | Send
  deriving (FromJSON, Eq, Generic, Ord, Show, ToJSON)

-- | Block's events, as seen by its forger.
data ForgerEvents a
  =  ForgerEvents
  { bfeHost       :: !Host
  , bfeBlock      :: !Hash
  , bfeBlockPrev  :: !Hash
  , bfeBlockNo    :: !BlockNo
  , bfeSlotNo     :: !SlotNo
  , bfeSlotStart  :: !SlotStart
  , bfeForged     :: !(Maybe a)
  , bfeAdopted    :: !(Maybe a)
  , bfeChainDelta :: !Int
  , bfeAnnounced  :: !(Maybe a)
  , bfeSending    :: !(Maybe a)
  , bfeErrs       :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

type ForgerEventsAbs = ForgerEvents UTCTime
type ForgerEventsRel = ForgerEvents NominalDiffTime

bfePrevBlock :: ForgerEvents a -> Maybe Hash
bfePrevBlock x = case bfeBlockNo x of
  0 -> Nothing
  _ -> Just $ bfeBlockPrev x

-- | Block's events, as seen by an observer.
data ObserverEvents a
  =  ObserverEvents
  { boeHost       :: !Host
  , boeBlock      :: !Hash
  , boeBlockNo    :: !BlockNo
  , boeSlotNo     :: !SlotNo
  , boeSlotStart  :: !SlotStart
  , boeNoticed    :: !(Maybe a)
  , boeRequested  :: !(Maybe a)
  , boeFetched    :: !(Maybe a)
  , boeAdopted    :: !(Maybe a)
  , boeChainDelta :: !Int
  , boeAnnounced  :: !(Maybe a)
  , boeSending    :: !(Maybe a)
  , boeErrs       :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

type ObserverEventsAbs = ObserverEvents UTCTime
type ObserverEventsRel = ObserverEvents NominalDiffTime

mbePhaseIndex :: Map Phase (MachBlockEvents a -> Maybe a)
mbePhaseIndex = Map.fromList
  [ (Notice,     mbeNoticed)
  , (Request,    mbeRequested)
  , (Fetch,      mbeAcquired)
  , (Forge,      mbeAcquired)
  , (Acquire,    mbeAcquired)
  , (Adopt,      mbeAdopted)
  , (Announce,   mbeAnnounced)
  , (Send,       mbeSending)
  ]

mbeGetProjection :: Phase -> (MachBlockEvents a -> Maybe a)
mbeGetProjection k =
  Map.lookup k mbePhaseIndex
  & fromMaybe (error $ "Unknown phase: " <> show k)

-- | Sum of observer and forger events alike.
data MachBlockEvents a
  = MFE (ForgerEvents a)
  | MOE (ObserverEvents a)
  | MBE  BPError

mbeForgP, mbeObsvP, mbeErrP :: MachBlockEvents a -> Bool
mbeForgP = \case
  MFE{} -> True
  _ -> False
mbeObsvP = \case
  MOE{} -> True
  _ -> False
mbeErrP = \case
  MBE{} -> True
  _ -> False

mapMbe :: (ForgerEvents a -> b) -> (ObserverEvents a -> b) -> (BPError -> b)
       -> MachBlockEvents a -> b
mapMbe f o e = \case
  MFE x -> f x
  MOE x -> o x
  MBE x -> e x

mapMbeErrs :: ([BPError] -> [BPError]) -> MachBlockEvents a -> MachBlockEvents a
mapMbeErrs f = mapMbe (\x -> MFE x { bfeErrs=f $ bfeErrs x } )
                      (\x -> MOE x { boeErrs=f $ boeErrs x } )
                      MBE

partitionMbes :: [MachBlockEvents a] -> ([ForgerEvents a], [ObserverEvents a], [BPError])
partitionMbes = go [] [] []
  where
    go :: [ForgerEvents a] -> [ObserverEvents a] -> [BPError] -> [MachBlockEvents a] -> ([ForgerEvents a], [ObserverEvents a], [BPError])
    go as bs cs [] = (reverse as, reverse bs, reverse cs)
    go as bs cs (MFE a:xs) = go (a:as) bs cs xs
    go as bs cs (MOE b:xs) = go as (b:bs) cs xs
    go as bs cs (MBE c:xs) = go as bs (c:cs) xs

errorMbes :: [MachBlockEvents a] -> [BPError]
errorMbes = go []
  where
    go :: [BPError] -> [MachBlockEvents a] -> [BPError]
    go cs [] = reverse cs
    go cs (MBE c:xs) = go (c:cs) xs
    go cs (_:xs)     = go    cs  xs

trimapMbe ::
     (ForgerEvents a -> ForgerEvents a)
  -> (ObserverEvents a -> ObserverEvents a)
  -> (BPError -> BPError)
  -> MachBlockEvents a -> MachBlockEvents a
trimapMbe f o e = mapMbe (MFE . f) (MOE . o) (MBE . e)

bimapMbe ::
     (ForgerEvents a -> ForgerEvents a)
  -> (ObserverEvents a -> ObserverEvents a)
  -> MachBlockEvents a -> MachBlockEvents a
bimapMbe f o = trimapMbe f o id

bimapMbe' ::
     (ForgerEvents   a -> Either BPError (ForgerEvents   a))
  -> (ObserverEvents a -> Either BPError (ObserverEvents a))
  -> MachBlockEvents a -> MachBlockEvents a
bimapMbe' f o = \case
  MFE x -> either MBE MFE (f x)
  MOE x -> either MBE MOE (o x)
  x@MBE{} -> x

ordBlockEv :: MachBlockEvents a -> MachBlockEvents a -> Ordering
ordBlockEv l r
  | (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) l r = GT
  | (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) r l = LT
  | mbeForgP l = GT
  | mbeForgP r = LT
  | mbeObsvP l = GT
  | mbeObsvP r = LT
  | otherwise  = EQ

mbeSlotStart :: MachBlockEvents a -> SlotStart
mbeSlotStart = mapMbe bfeSlotStart boeSlotStart (SlotStart . const zeroUTCTime)

mbeNoticed, mbeRequested, mbeAcquired, mbeAdopted, mbeAnnounced, mbeSending :: MachBlockEvents a -> Maybe a
mbeNoticed   = mapMbe (const Nothing)  boeNoticed   (const Nothing)
mbeRequested = mapMbe (const Nothing)  boeRequested (const Nothing)
mbeAcquired  = mapMbe bfeForged        boeFetched   (const Nothing)
mbeAdopted   = mapMbe bfeAdopted       boeAdopted   (const Nothing)
mbeAnnounced = mapMbe bfeAnnounced     boeAnnounced (const Nothing)
mbeSending   = mapMbe bfeSending       boeSending   (const Nothing)

mbeBlock :: MachBlockEvents a -> Hash
mbeBlock = mapMbe bfeBlock boeBlock eBlock

mbeBlockNo :: MachBlockEvents a -> BlockNo
mbeBlockNo = mapMbe bfeBlockNo boeBlockNo (const (-1))

mbeError :: MachBlockEvents a -> Maybe BPError
mbeError = mapMbe (const Nothing) (const Nothing) Just

mbeFailed :: MachBlockEvents a -> Bool
mbeFailed = isJust . mbeError

-- | Machine's private view of all the blocks.
type MachBlockMap a
  =  Map.Map Hash (MachBlockEvents a)

blockMapMaxBlock :: MachBlockMap a -> MachBlockEvents a
blockMapMaxBlock = maximumBy ordBlockEv . Map.elems

blockMapBlock :: Hash -> MachBlockMap a -> MachBlockEvents a
blockMapBlock h =
  fromMaybe (error $ "Invariant failed:  missing hash " <> show h) . Map.lookup h

-- | A completed, compactified version of ObserverEvents.
data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !SlotStart
  , boNoticed    :: !NominalDiffTime
  , boRequested  :: !NominalDiffTime
  , boFetched    :: !NominalDiffTime
  , boAdopted    :: !(Maybe NominalDiffTime)
  , boChainDelta :: !Int -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe NominalDiffTime)
  , boSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beForger       :: !Host
  , beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beSlotStart    :: !SlotStart
  , beForged       :: !NominalDiffTime
  , beAdopted      :: !NominalDiffTime
  , beChainDelta   :: !Int -- ^ ChainDelta during adoption
  , beAnnounced    :: !NominalDiffTime
  , beSending      :: !NominalDiffTime
  , beObservations :: [BlockObservation]
  , beOtherBlocks  :: [Hash]
  , beErrors       :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

instance RenderTimeline BlockEvents where
  rtFields =
    --  Width LeftPad
    [ Field 5 0 "block"       "block" "no."    $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"    "abs."  "slot#"  $ IWord64 (unSlotNo . beSlotNo)
    , Field 6 0 "hash"        "block" "hash"   $ IText   (shortHash . beBlock)
    , Field 6 0 "hashPrev"    "prev"  "hash"   $ IText   (shortHash . beBlockPrev)
    , Field 5 0 "peer.observ" "valid" "obsrv"  $ IInt    (length . beObservations)
    , Field 5 0 "errors"      "all"   "errs"   $ IInt    (length . beErrors)
    , Field 5 0 "forks"       ""      "forks"  $ IInt    (count bpeIsFork . beErrors)
    , Field 5 0 "missAdopt"   (m!!0) "adopt"   $ IInt    (count (bpeIsMissing Adopt) . beErrors)
    , Field 5 0 "missAnnou"   (m!!1) "annou"   $ IInt    (count (bpeIsMissing Announce) . beErrors)
    , Field 5 0 "missSend"    (m!!2) "send"    $ IInt    (count (bpeIsMissing Send) . beErrors)
    , Field 5 0 "negAnnou"    (n!!0) "annou"   $ IInt    (count (bpeIsNegative Announce) . beErrors)
    , Field 5 0 "negSend"     (n!!1) "send"    $ IInt    (count (bpeIsNegative Send) . beErrors)
    ]
   where
     m = nChunksEachOf 3 6 "Missing phase"
     n = nChunksEachOf 2 6 "Negative phase"
     count :: (a -> Bool) -> [a] -> Int
     count f = length . filter f
  rtCommentary BlockEvents{..} = ("    " <>) . T.pack . show <$> beErrors

mapChainToForgerEventCDF ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockEvents -> Maybe NominalDiffTime)
  -> Distribution Float NominalDiffTime
mapChainToForgerEventCDF percs cbe proj =
  computeDistribution percs (mapMaybe proj cbe)

mapChainToPeerBlockObservationCDFs ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockObservation -> Maybe NominalDiffTime)
  -> String
  -> (Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
mapChainToPeerBlockObservationCDFs percs cbe proj desc =
  (means, covs)
 where
   means, covs :: Distribution Float NominalDiffTime
   (,) means covs = computeDistributionStats desc
                      (fmap realToFrac <$> allDistributions)
                    & either error id
                    & join (***) (fmap realToFrac)

   allDistributions :: [Distribution Float NominalDiffTime]
   allDistributions = computeDistribution percs <$> allObservations

   allObservations :: [[NominalDiffTime]]
   allObservations = blockObservations <$> cbe

   blockObservations :: BlockEvents -> [NominalDiffTime]
   blockObservations be = mapMaybe proj (beObservations be)

blockProp :: ChainInfo -> [(JsonLogfile, [LogObject])] -> IO BlockPropagation
blockProp ci xs = do
  putStrLn ("blockProp: recovering block event maps" :: String)
  doBlockProp =<< mapConcurrently (pure
                                   . fmap deltifyEvents
                                   . blockEventMapsFromLogObjects ci) xs

doBlockProp :: [MachBlockMap NominalDiffTime] -> IO BlockPropagation
doBlockProp eventMaps = do
  putStrLn ("tip block: " <> show tipBlock :: String)
  putStrLn ("chain length: " <> show (length chain) :: String)
  pure $ BlockPropagation
    (forgerEventsCDF    (Just . beForged))
    (forgerEventsCDF    (\x -> if beChainDelta x == 1 then Just (beAdopted x)
                               else Nothing))
    (forgerEventsCDF    (Just . beAnnounced))
    (forgerEventsCDF    (Just . beSending))
    (observerEventsCDFs (Just . boNoticed) "peer noticed")
    (observerEventsCDFs (Just . boRequested) "peer requested")
    (observerEventsCDFs (Just . boFetched) "peer fetched")
    (observerEventsCDFs (\x -> if boChainDelta x == 1 then boAdopted x
                               else Nothing) "peer adopted")
    (observerEventsCDFs boAnnounced "peer announced")
    (observerEventsCDFs boSending   "peer sending")
    chain
 where
   forgerEventsCDF    = mapChainToForgerEventCDF           stdPercentiles chain
   observerEventsCDFs = mapChainToPeerBlockObservationCDFs stdPercentiles chain

   chain          :: [BlockEvents]
   chain          = rebuildChain eventMaps tipHash
   heightMap      :: Map BlockNo (Set Hash)
   heightMap      = foldr (\em acc ->
                             Map.foldr
                             (\mbe -> Map.alter
                                      (maybe (Just $ Set.singleton (mbeBlock mbe))
                                             (Just . Set.insert (mbeBlock mbe)))
                                      (mbeBlockNo mbe))
                             acc em)
                    mempty eventMaps
   tipBlock       = getBlockForge eventMaps tipHash
   tipHash        = rewindChain eventMaps 1 (mbeBlock finalBlockEv)
   finalBlockEv   = maximumBy ordBlockEv $ blockMapMaxBlock <$> eventMaps

   rewindChain :: [MachBlockMap a] -> Int -> Hash -> Hash
   rewindChain eventMaps count tip = go tip count
    where go tip = \case
            0 -> tip
            n -> go (bfeBlockPrev $ getBlockForge eventMaps tip) (n - 1)

   getBlockForge :: [MachBlockMap a] -> Hash -> ForgerEvents a
   getBlockForge xs h =
     mapMaybe (Map.lookup h) xs
     & find mbeForgP
     & fromMaybe
        (error $ mconcat
         [ "Invariant failed: couldn't find a forge for hash ", show h
         , "\nErrors:\n", show (intercalate "\n" $ fmap show $ errorMbes $ mapMaybe (Map.lookup h) xs)
         ])
     & mapMbe id (error "Silly invariant failed.") (error "Silly invariant failed.")

   rebuildChain :: [MachBlockMap NominalDiffTime] -> Hash -> [BlockEvents]
   rebuildChain machBlockMaps tip = go (Just tip) []
    where go Nothing  acc = acc
          go (Just h) acc =
            case partitionMbes $ mapMaybe (Map.lookup h) machBlockMaps of
              ([], _, ers) -> error $ mconcat
                [ "No forger for hash ", show h
                , "\nErrors:\n"
                ] ++ intercalate "\n" (show <$> ers)
              blkEvs@(forgerEv:_, oEvs, ers) ->
                go (bfePrevBlock forgerEv) (liftBlockEvents forgerEv oEvs ers : acc)

   liftBlockEvents :: ForgerEventsRel -> [ObserverEvents NominalDiffTime] -> [BPError] -> BlockEvents
   liftBlockEvents ForgerEvents{..} os errs =
     BlockEvents
     { beForger     = bfeHost
     , beBlock      = bfeBlock
     , beBlockPrev  = bfeBlockPrev
     , beBlockNo    = bfeBlockNo
     , beSlotNo     = bfeSlotNo
     , beSlotStart  = bfeSlotStart
     , beForged     = bfeForged    & handleMiss "Δt Forged"
     , beAdopted    = bfeAdopted   & handleMiss "Δt Adopted (forger)"
     , beChainDelta = bfeChainDelta
     , beAnnounced  = bfeAnnounced & handleMiss "Δt Announced (forger)"
     , beSending    = bfeSending   & handleMiss "Δt Sending (forger)"
     , beObservations = catMaybes $
       os <&> \ObserverEvents{..}->
         BlockObservation
           <$> Just boeHost
           <*> Just bfeSlotStart
           <*> boeNoticed
           <*> boeRequested
           <*> boeFetched
           <*> Just boeAdopted
           <*> Just boeChainDelta
           <*> Just boeAnnounced
           <*> Just boeSending
     , beOtherBlocks = otherBlocks
     , beErrors =
         errs
         <> (fail' bfeBlock . BPEFork <$> otherBlocks)
         <> bfeErrs
         <> concatMap boeErrs os
     }
    where
      otherBlocks = Map.lookup bfeBlockNo heightMap
                    & handleMiss "height map"
                    & Set.delete bfeBlock
                    & Set.toList
      fail' :: Hash -> BPErrorKind -> BPError
      fail' hash desc = BPError hash Nothing Nothing desc

      handleMiss :: String -> Maybe a -> a
      handleMiss slotDesc = fromMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show bfeHost
       , " -- missing: ", slotDesc
       ]

-- | Given a single machine's log object stream, recover its block map.
blockEventMapsFromLogObjects :: ChainInfo -> (JsonLogfile, [LogObject]) -> MachBlockMap UTCTime
blockEventMapsFromLogObjects ci (f@(unJsonLogfile -> fp), xs) =
  trace ("processing " <> fp)
  $ if Map.size machBlockMap == 0
    then error $ mconcat
         ["No block events in ",fp," : ","LogObject count: ",show (length xs)]
    else machBlockMap
 where
   machBlockMap = foldl (blockPropMachEventsStep ci f) mempty xs

blockPropMachEventsStep :: ChainInfo -> JsonLogfile -> MachBlockMap UTCTime -> LogObject -> MachBlockMap UTCTime
blockPropMachEventsStep ci (JsonLogfile fp) bMap lo = case lo of
  -- 0. Notice (observer only)
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = Map.lookup loBlock bMap
    in if isJust mbe0 then bMap else
      (MOE $
       ObserverEvents
         loHost loBlock loBlockNo loSlotNo
         (slotStart ci loSlotNo) (Just loAt)
         Nothing Nothing Nothing 0 Nothing Nothing [])
      & doInsert loBlock
  -- 1. Request (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientRequested{loBlock,loLength}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Request)
    in if isJust (mbeRequested mbe0) then bMap else
      bimapMbe'
      (const . Left $ fail' loBlock $ BPEUnexpectedForForger Request)
      (\x -> Right x { boeRequested=Just loAt, boeChainDelta=loLength `max` boeChainDelta x })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Fetch (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Fetch)
    in if isJust (mbeAcquired mbe0) then bMap else
      bimapMbe'
      (const . Left $ fail' loBlock (BPEUnexpectedForForger Fetch))
      (\x -> Right x { boeFetched=Just loAt })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Forge (forger only)
  LogObject{loAt, loHost, loBody=LOBlockForged{loBlock,loPrev,loBlockNo,loSlotNo}} ->
    Map.lookup loBlock bMap
    <&> bimapMbe'
          (const.Left $
           BPError loBlock (Just fp) (Just lo) BPEDuplicateForge)
          (const.Left $
           BPError loBlock (Just fp) (Just lo) (BPEUnexpectedForObserver Forge))
    & fromMaybe
      (MFE $ ForgerEvents
        loHost loBlock loPrev loBlockNo loSlotNo
        (slotStart ci loSlotNo) (Just loAt)
        Nothing 0 Nothing Nothing [])
    & doInsert loBlock
  -- 3. Adopt
  LogObject{loAt, loHost, loBody=LOBlockAddedToCurrentChain{loBlock,loLength}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Adopt)
    in if isJust (mbeAdopted mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeAdopted=Just loAt, bfeChainDelta=loLength })
      (\x -> x { boeAdopted=Just loAt, boeChainDelta=loLength `max` boeChainDelta x})
      mbe0
      & doInsert loBlock
  -- 4. Announce
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Announce)
    in if isJust (mbeAnnounced mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeAnnounced=Just loAt })
      (\x -> x { boeAnnounced=Just loAt })
      mbe0
      & doInsert loBlock
  -- 5. Sending started
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Send)
    in if isJust (mbeSending mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeSending=Just loAt })
      (\x -> x { boeSending=Just loAt })
      mbe0
      & doInsert loBlock
  _ -> bMap
 where
   fail' :: Hash -> BPErrorKind -> BPError
   fail' hash desc = BPError hash (Just fp) (Just lo) desc

   fail :: Hash -> BPErrorKind -> MachBlockEvents a
   fail hash desc = MBE $ fail' hash desc

   doInsert :: Hash -> MachBlockEvents UTCTime -> MachBlockMap UTCTime
   doInsert k x = Map.insert k x bMap

deltifyEvents :: MachBlockEvents UTCTime -> MachBlockEvents NominalDiffTime
deltifyEvents (MBE e) = MBE e
deltifyEvents (MFE x@ForgerEvents{..}) =
  MFE x
  { bfeForged    = bfeForged <&> (`sinceSlot` bfeSlotStart)
  , bfeAdopted   = diffUTCTime <$> bfeAdopted   <*> bfeForged
  , bfeAnnounced = diffUTCTime <$> bfeAnnounced <*> bfeAdopted
  , bfeSending   = diffUTCTime <$> bfeSending   <*> bfeAnnounced
  } & \case
  v@(MFE x') -> MFE x' { bfeErrs = collectEventErrors v
                         [Forge, Adopt, Announce, Send] }
  _ -> error "Impossible"
deltifyEvents (MOE x@ObserverEvents{..}) =
  MOE x
  { boeNoticed   = boeNoticed <&> (`sinceSlot` boeSlotStart)
  , boeRequested = diffUTCTime <$> boeRequested <*> boeNoticed
  , boeFetched   = diffUTCTime <$> boeFetched   <*> boeRequested
  , boeAdopted   = diffUTCTime <$> boeAdopted   <*> boeFetched
  , boeAnnounced = diffUTCTime <$> boeAnnounced <*> boeAdopted
  , boeSending   = diffUTCTime <$> boeSending   <*> boeAnnounced
  } & \case
  v@(MOE x') -> MOE x' { boeErrs = collectEventErrors v
                         [Notice, Request, Fetch, Adopt, Announce, Send] }
  _ -> error "Impossible"

collectEventErrors :: MachBlockEvents NominalDiffTime -> [Phase] -> [BPError]
collectEventErrors mbe phases =
  [ BPError (mbeBlock mbe) Nothing Nothing $
    case (miss, proj) of
      (,) True _       -> BPEMissingPhase phase
      (,) _ (Just neg) -> BPENegativePhase phase neg
      _ -> error "Impossible."
  | phase <- phases
  , let proj = mbeGetProjection phase mbe
  , let miss = isNothing proj
  , let neg  = ((< 0) <$> proj) == Just True
  , miss || neg
  ]
   -- deltaTStrict :: Phase -> UTCTime -> MachBlockEvents NominalDiffTime -> [Phase] -> Either (MachBlockEvents NominalDiffTime) ([BPError], NominalDiffTime)
   -- deltaTStrict desc t mbe = deltaT desc t mbe . fmap Right

   -- deltaT :: Phase -> UTCTime -> MachBlockEvents NominalDiffTime -> [Either Phase Phase] -> Either (MachBlockEvents NominalDiffTime) ([BPError], NominalDiffTime)
   -- deltaT ph t mbe mdtProjs =
   --   maybeReportNegativeDelta . fmap (t `diffUTCTime`) <$>
   --     foldM (\(ers,tv) eiProjs@(join either id -> proj) ->
   --               either
   --                (Right .
   --                 maybe (fail' block (ph `BPEBefore` proj):ers, 0)
   --                       (ers,))
   --                (maybe (Left $ fail block (ph `BPEBefore` proj))
   --                       (Right . (ers,)))
   --                (join bimap (($ mbe) . mbeGetProjection) eiProjs)
   --              <&> fmap (flip addUTCTime tv))
   --           ([], unSlotStart slStart)
   --           mdtProjs
   --  where
   --    slStart = mbeSlotStart mbe
   --    block   = mbeBlock mbe

   --    maybeReportNegativeDelta
   --      :: ([BPError], NominalDiffTime) -> ([BPError], NominalDiffTime)
   --    maybeReportNegativeDelta v@(ers, d) =
   --      if d >= 0 then v else
   --      ((fail' block $
   --        BPENegativeDelta
   --          ph t slStart d
   --          (join either (id &&& (fromMaybe 0 . ($ mbe) . mbeGetProjection))
   --            <$> mdtProjs))
   --        :ers, d)
