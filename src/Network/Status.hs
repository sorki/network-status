{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Status where

import Data.Default
import Data.Maybe
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.ByteString as S

import System.Linux.RTNetlink
import System.Linux.RTNetlink.Address as A
import System.Linux.RTNetlink.Link
import System.Linux.RTNetlink.Scope
import System.Linux.RTNetlink.Route
import System.Socket.Protocol.RTNetlink

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import Text.Pretty.Simple
import Debug.Trace
import Safe

type IfName = S.ByteString

type IfMap = M.Map IfName Interface

data Interface = Interface {
      ifMac  :: LinkEther
    , ifIPsV4 :: [InetAddress]
    , ifIPsV6 :: [Inet6Address]
    , ifGwV4 :: Maybe InetAddress
    , ifGwV6 :: Maybe Inet6Address
    , ifRoutesV4 :: [InetAddress]
    , ifRoutesV6 :: [Inet6Address]
  }
  deriving (Eq, Show)

instance Default Interface where
  def = Interface {
      ifMac  = LinkEther 0 0 0 0 0 0
    , ifIPsV4 = []
    , ifIPsV6 = []
    , ifGwV4 = Nothing
    , ifGwV6 = Nothing
    , ifRoutesV4 = []
    , ifRoutesV6 = []
  }

data Event =
    IFNew IfName Interface
  | IFDel IfName
  | IFChange IfName Op
  | IFInfo IfMap
  deriving (Eq, Show)

data Op =
    AddIP     InetAddress
  | AddIP6    Inet6Address
  | DelIP     InetAddress
  | DelIP6    Inet6Address
  | AddGW     InetAddress
  | AddGW6    Inet6Address
  | DelGW
  | DelGW6
  | AddRoute  InetAddress
  | AddRoute6 Inet6Address
  | DelRoute  InetAddress
  | DelRoute6 Inet6Address
  deriving (Eq, Show)

data Config = Config {
    cfgLinkFilter   :: (LinkIndex, LinkName, LinkState, LinkEther) -> Bool
  , cfgIPv4Filter   :: IfInetAddress -> Bool
  , cfgIPv6Filter   :: IfInet6Address -> Bool
  , cfgRouteFilter  :: RouteInfo -> Bool
  , cfgRoute6Filter :: Route6Info -> Bool
  , cfgDebug        :: Bool
  }

instance Default Config where
  def = Config {
    cfgLinkFilter   =  \(_ix, name, state, _ether) -> state == Up && name /= LinkName "lo"
  , cfgIPv4Filter   = (== (IfScope ScopeUniverse)) . ifInetScope
  , cfgIPv6Filter   = (== (IfScope ScopeUniverse)) . ifInet6Scope
  , cfgRouteFilter  = (== RouteTableMain) . routeTable
  , cfgRoute6Filter = (== RouteTableMain) . route6Table
  , cfgDebug        = False
  }

handle :: Event -> IfMap -> IfMap
handle (IFChange name op) s = M.update (mkUpdate op) name s
handle (IFNew name ifc)   s = M.insert name ifc s
handle (IFDel name)       s = M.delete name s
handle (IFInfo ifmap)     s = ifmap

mkUpdate (AddIP ip)     x = Just $ x { ifIPsV4 = nub $ (ifIPsV4 x) ++ [ip] }
mkUpdate (DelIP ip)     x = Just $ x { ifIPsV4 = filter (/=ip) (ifIPsV4 x) }
mkUpdate (AddIP6 ip)    x = Just $ x { ifIPsV6 = nub $ (ifIPsV6 x) ++ [ip] }
mkUpdate (DelIP6 ip)    x = Just $ x { ifIPsV6 = filter (/=ip) (ifIPsV6 x) }
mkUpdate (AddGW ip)     x = Just $ x { ifGwV4 = Just ip }
mkUpdate  DelGW         x = Just $ x { ifGwV4 = Nothing }
mkUpdate (AddGW6 ip)    x = Just $ x { ifGwV6 = Just ip }
mkUpdate  DelGW6        x = Just $ x { ifGwV6 = Nothing }
mkUpdate (AddRoute ip)  x = Just $ x { ifRoutesV4 = nub $ (ifRoutesV4 x) ++ [ip] }
mkUpdate (DelRoute ip)  x = Just $ x { ifRoutesV4 = filter (/=ip) (ifRoutesV4 x) }
mkUpdate (AddRoute6 ip) x = Just $ x { ifRoutesV6 = nub $ (ifRoutesV6 x) ++ [ip] }
mkUpdate (DelRoute6 ip) x = Just $ x { ifRoutesV6 = filter (/=ip) (ifRoutesV6 x) }
mkUpdate _ x = Just x

filterLinks :: [(LinkIndex, LinkName, LinkState, LinkEther)] -> [(LinkIndex, LinkName, LinkState, LinkEther)]
filterLinks = filter (\(_ix, name, state, _ether) -> state == Up && name /=  LinkName "lo")

globalAddrs :: [IfInetAddress] -> [IfInetAddress]
globalAddrs = filter ((== (IfScope ScopeUniverse)) . ifInetScope)

globalAddrs6 :: [IfInet6Address] -> [IfInet6Address]
globalAddrs6 = filter ((== (IfScope ScopeUniverse)) . ifInet6Scope)

filterByIfIndex ix = filter ((== RouteOIFIndex ix) . routeOIFIndex )
filterMainTable = filter ((== RouteTableMain) . routeTable)

filterByIfIndex6 ix = filter ((== RouteOIFIndex ix) . route6OIFIndex )
filterMainTable6 = filter ((== RouteTableMain) . route6Table)

--status :: IO (M.Map S.ByteString Status)
unionAddrs lmap v4s v6s r4s r6s = do
  let v4s' = M.fromListWith merge $ catMaybes $ map (repackAddr  lmap r4s) v4s
      v6s' = M.fromListWith merge $ catMaybes $ map (repackAddr6 lmap r6s) v6s
    in M.unionWith merge v4s' v6s'
  where
    repackAddr ls routes IfInetAddress{..} =
      let ix = A.ifIndex $ ifInetIfIndex
          froutes = filterMainTable $ filterByIfIndex ix routes
          rs = map (\(RouteAddress a) -> a) $ catMaybes $ map routeDstAddress froutes
          gw = fmap (\(RouteGateway a) -> a) $ headMay $ catMaybes $ map routeGateway froutes
          in case byIndex ls ix of
            Nothing -> Nothing
            Just (name, mac) -> Just
              (name, def { ifMac = mac
                         , ifIPsV4 = [ifInetAddress]
                         , ifGwV4 = gw
                         , ifRoutesV4 = rs
                         })

    repackAddr6 ls routes IfInet6Address{..} =
      let ix = A.ifIndex $ ifInet6IfIndex
          froutes = filterMainTable6 $ filterByIfIndex6 ix routes
          rs = map (\(Route6Address a) -> a) $ catMaybes $ map route6DstAddress froutes
          gw = fmap (\(Route6Gateway a) -> a) $ headMay $ catMaybes $ map route6Gateway froutes
          in case byIndex ls ix of
            Nothing -> Nothing
            Just (name, mac) -> Just
              (name, def { ifMac = mac
                         , ifIPsV6 = [ifInet6Address]
                         , ifGwV6 = gw
                         , ifRoutesV6 = rs })

    merge a b = mergeGws b $ mergeRoutes b $ mergeIPs b a

    mergeIPs b a = a {
        ifIPsV4 = nub (ifIPsV4 a ++ ifIPsV4 b)
      , ifIPsV6 = nub (ifIPsV6 a ++ ifIPsV6 b) }

    mergeRoutes b a = a {
        ifRoutesV4 = nub (ifRoutesV4 a ++ ifRoutesV4 b)
      , ifRoutesV6 = nub (ifRoutesV6 a ++ ifRoutesV6 b) }

    mergeGws b a | ifGwV4 a == Nothing = a { ifGwV4 = (ifGwV4 b) }
    mergeGws b a | ifGwV6 a == Nothing = a { ifGwV6 = (ifGwV6 b) }
    mergeGws _ a | otherwise = a

linkMap :: [(LinkIndex, LinkName, LinkState, LinkEther)] -> M.Map LinkIndex (LinkName, LinkEther)
linkMap x = M.fromList (repackLinks x)
  where
    repackLinks x = map (\(ix, name, _state, mac) -> (ix, (name, mac))) x

byIndex ls i = case M.lookup (LinkIndex i) ls of
  Nothing -> Nothing
  Just (LinkName lname, mac) -> Just (lname, mac)

queryStateLinks conf@Config{..} = do
  ls  :: [(LinkIndex, LinkName, LinkState, LinkEther)] <- (filter cfgLinkFilter . map untupLink) <$> dump AnyLink
  v4s :: [IfInetAddress]  <- filter cfgIPv4Filter <$> dump AnyInterface
  v6s :: [IfInet6Address] <- filter cfgIPv6Filter <$> dump AnyInterface

  r4s :: [RouteInfo] <- filter cfgRouteFilter <$> dump AnyRoute
  r6s :: [Route6Info] <- filter cfgRoute6Filter <$> dump AnyRoute

  let lmap = linkMap ls
  return $ (lmap, unionAddrs lmap v4s v6s r4s r6s)

-- XXX -> s/watch/run
watch = runConf def

runConf conf@Config{..} = do
  chan <- atomically $ newTChan

  clientState <- atomically $ newTVar M.empty

  async $ forever $ do
    res <- atomically $ do
      event <- readTChan chan
      modifyTVar clientState $ handle event
      d <- readTVar clientState
      return (event, d)

    pPrint res
    return ()

  runRTNLGroups [
      RTNetlinkGroupLink
    , RTNetlinkGroupIPv4IfAddr
    , RTNetlinkGroupIPv4Route
    , RTNetlinkGroupIPv6IfAddr
    , RTNetlinkGroupIPv6Route
    ] $ do

      (initLinkMap, initState) <- queryStateLinks conf

      (localState, lmap) <- liftSTM $ do
        writeTChan chan $ IFInfo initState
        (,) <$> newTVar initState <*> newTVar initLinkMap

      let
          apply x@(IFChange name op) = do
            curr <- readTVar localState

            when cfgDebug $ trace (show x) (return ())

            case M.lookup name curr of
              -- if this is an unknown link emit IFNew instead
              Nothing -> do
                case op of
                  (AddIP  ip)     -> hasMac name $ \mac -> apply (IFNew name $ def {ifMac = mac, ifIPsV4 = [ip]})
                  (AddIP6 ip6)    -> hasMac name $ \mac -> apply (IFNew name $ def {ifMac = mac, ifIPsV6 = [ip6]})
                  (AddRoute  ip)  -> hasMac name $ \mac -> apply (IFNew name $ def {ifMac = mac, ifRoutesV4 = [ip]})
                  (AddRoute6 ip6) -> hasMac name $ \mac -> apply (IFNew name $ def {ifMac = mac, ifRoutesV6 = [ip6]})
                  _ -> return Nothing

              Just Interface{..} -> do
                case op of
                  (AddIP  ip)     -> noDupes (ip `elem` ifIPsV4) x
                  (AddIP6 ip6)    -> noDupes (ip6 `elem` ifIPsV6) x
                  (AddRoute  ip)  -> noDupes (ip `elem` ifRoutesV4) x
                  (AddRoute6 ip6) -> noDupes (ip6 `elem` ifRoutesV6) x
                  _ -> app x >> return (Just x)

          apply x = app x >> return (Just x)

          app = modifyTVar localState . handle

          noDupes True  _ = return Nothing
          noDupes False x = app x >> return (Just x)

          emit (Just x) = writeTChan chan $ x
          emit Nothing = return ()

          go x = do
            y <- apply x
            emit y

          withLink i f = do
            lm <- readTVar lmap
            maybe (return ()) (\(LinkName name, _) -> f name) $ M.lookup (LinkIndex i) lm

          hasLink i f = withLink (A.ifIndex i) f
          hasLinkR (RouteOIFIndex i) f = withLink i f

          hasMac n f = do
            lm <- readTVar lmap
            case M.toList $ M.filter (\(LinkName name, _) -> name == n) lm of
              -- temporarily there might be more links with the same name
              (_, (_, e)):xs -> f e -- not exactly safe, we should at least take the highest index
              _ -> return Nothing

          hasDst (Just (RouteAddress a)) f = f a
          hasDst Nothing _ = return ()

          hasDst6 (Just (Route6Address a)) f = f a
          hasDst6 Nothing _ = return ()

          hasGw (Just (RouteGateway a)) f = f a
          hasGw Nothing _ = return ()

          hasGw6 (Just (Route6Gateway a)) f = f a
          hasGw6 Nothing _ = return ()

      forever $ do
        ls     :: [(LinkIndex, LinkName, LinkState, LinkEther)] <- (filter cfgLinkFilter . map untupLink)  <$> getBacklog
        lsDel  :: [LinkDeleted (LinkIndex, LinkName)]           <- getBacklog

        v4s    :: [IfInetAddress]                               <- filter cfgIPv4Filter   <$> getBacklog
        v6s    :: [IfInet6Address]                              <- filter cfgIPv6Filter   <$> getBacklog

        v4sDel :: [IfDeleted IfInetAddress]                     <- getBacklog
        v6sDel :: [IfDeleted IfInet6Address]                    <- getBacklog

        r4s    :: [RouteInfo]                                   <- filter cfgRouteFilter  <$> getBacklog
        r6s    :: [Route6Info]                                  <- filter cfgRoute6Filter <$> getBacklog

        r4sDel :: [RouteDeleted RouteInfo]                      <- getBacklog
        r6sDel :: [RouteDeleted Route6Info]                     <- getBacklog

        let p [] = return()
            p x = when cfgDebug $ liftIO $ pPrint x
        p ls
        p lsDel
        p v4s
        p v4sDel
        p v6s
        p v6sDel
        p r4s
        p r6s

        liftSTM $ do
          forM_ lsDel  $ \(LinkDeleted (idx, LinkName name)) -> do
            modifyTVar lmap (M.delete idx)
            go $ IFDel name

          forM_ ls     $ \(idx, name, _state, mac)  -> do
            modifyTVar lmap (M.insert idx (name, mac))

          forM_ v4sDel $ \(IfDeleted IfInetAddress{..})  -> hasLink ifInetIfIndex $ \n ->
            go $ IFChange n $ DelIP ifInetAddress
          forM_ v4s    $  \IfInetAddress{..}             -> hasLink ifInetIfIndex $ \n ->
            go $ IFChange n $ AddIP ifInetAddress

          forM_ v6sDel $ \(IfDeleted IfInet6Address{..}) -> hasLink ifInet6IfIndex $ \n ->
            go $ IFChange n $ DelIP6 ifInet6Address
          forM_ v6s    $  \IfInet6Address{..}            -> hasLink ifInet6IfIndex $ \n ->
            go $ IFChange n $ AddIP6 ifInet6Address

          forM_ r4sDel $ \(RouteDeleted RouteInfo{..})   -> do
            hasDst routeDstAddress $ \dst -> hasLinkR routeOIFIndex $ \n -> go $ IFChange n $ DelRoute dst
            hasGw  routeGateway    $ \gw  -> hasLinkR routeOIFIndex $ \n -> go $ IFChange n $ DelGW

          forM_ r4s    $ \(RouteInfo{..})                -> do
            hasDst routeDstAddress $ \dst -> hasLinkR routeOIFIndex $ \n -> go $ IFChange n $ AddRoute dst
            hasGw  routeGateway    $ \gw  -> hasLinkR routeOIFIndex $ \n -> go $ IFChange n $ AddGW gw

          forM_ r6sDel $ \(RouteDeleted Route6Info{..})  -> do
            hasDst6 route6DstAddress $ \dst -> hasLinkR route6OIFIndex $ \n -> go $ IFChange n $ DelRoute6 dst
            hasGw6  route6Gateway    $ \gw  -> hasLinkR route6OIFIndex $ \n -> go $ IFChange n $ DelGW6

          forM_ r6s    $ \(Route6Info{..})               -> do
            hasDst6 route6DstAddress $ \dst -> hasLinkR route6OIFIndex $ \n -> go $ IFChange n $ AddRoute6 dst
            hasGw6  route6Gateway    $ \gw  -> hasLinkR route6OIFIndex $ \n -> go $ IFChange n $ AddGW6 gw

        clearBacklog
        liftIO $ threadDelay 100000

liftSTM = liftIO . atomically
untupLink (a, (b, (c, d))) = (a, b, c, d)
