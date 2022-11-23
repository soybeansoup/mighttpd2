{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module WaiApp (fileCgiApp) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (preconditionFailed412, movedPermanently301, urlDecode, badRequest400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Internal
import Network.Wai.Application.Classic


import Program.Mighty

data Perhaps a = Found a | Redirect | Fail

fileCgiApp :: ClassicAppSpec -> FileAppSpec -> CgiAppSpec -> RevProxyAppSpec
           -> RouteDBRef -> ReqRef -> Application
fileCgiApp cspec filespec cgispec revproxyspec rdr reqRef req respond
  | dotFile = do
        let st = badRequest400
        fastResponse respond st defaultHeader "Bad Request\r\n"
  | otherwise = do
    reqSt <- getRRState reqRef
    setRRState reqRef
    um <- readRouteDBRef rdr
    print reqSt
    print um
    case mmp um reqSt of
        Fail -> do
            let st = preconditionFailed412
            fastResponse respond st defaultHeader "Precondition Failed\r\n"
        Redirect -> do
            let st = movedPermanently301
                hdr = defaultHeader ++ redirectHeader req'
            fastResponse respond st hdr "Moved Permanently\r\n"
        Found (RouteFile  src dst) ->
            fileApp cspec filespec (FileRoute src dst) req' respond
        Found (RouteRedirect src dst) ->
            redirectApp cspec (RedirectRoute src dst) req' respond
        Found (RouteCGI   src dst) ->
            cgiApp cspec cgispec (CgiRoute src dst) req' respond
        Found (RouteRevProxy src dst dom prt) ->
            revProxyApp cspec revproxyspec (RevProxyRoute src dst dom (if even reqSt then 55002 else 55000)) req respond



            --(naturalToInt prt)


 
  where
    (host, _) = hostPort req
    rawpath = rawPathInfo req
    path = urlDecode False rawpath
    dotFile = BS.isPrefixOf "." rawpath || BS.isInfixOf "/." rawpath
    mmp um reqSt = case getBlock host um of
        Nothing    -> Fail
        Just block -> getRoute path block reqSt
    fastResponse resp st hdr body = resp $ responseLBS st hdr body
    defaultHeader = [("Content-Type", "text/plain")]
    req' = req { rawPathInfo = path } -- FIXME

getBlock :: ByteString -> RouteDB -> Maybe [Route]
getBlock _ [] = Nothing
getBlock key (Block doms maps : ms)
  | "*" `elem` doms = Just maps
  | key `elem` doms = Just maps
  | otherwise       = getBlock key ms

getRoute :: ByteString -> [Route] -> Int -> Perhaps Route
getRoute _ [] _               = Fail
getRoute key (m:ms) reqSt
  | src `isPrefixOf` key     = Found m
  | src `isMountPointOf` key = Redirect
  | otherwise                = getRoute key ms reqSt
  where
    src = routeSource m

routeSource :: Route -> Src
routeSource (RouteFile     src _)     = src
routeSource (RouteRedirect src _)     = src
routeSource (RouteCGI      src _)     = src
routeSource (RouteRevProxy src _ _ _) = src

isPrefixOf :: Path -> ByteString -> Bool
isPrefixOf src key = src `BS.isPrefixOf` key

isMountPointOf :: Path -> ByteString -> Bool
isMountPointOf src key = hasTrailingPathSeparator src
                      && BS.length src - BS.length key == 1
                      && key `BS.isPrefixOf` src



