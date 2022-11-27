{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Program.Mighty.Route (
  -- * Paring a routing file
    parseRoute
  -- * Types
  , RouteDB
  , Route(..)
  , Block(..)
  , Src
  , Dst
  , Domain
  , Port
  , Weight
  -- * RouteDBRef
  , RouteDBRef
  , ReqRef
  , newRouteDBRef
  , readRouteDBRef
  , writeRouteDBRef
  , getRRState
  , setRRState
  , initRRState
  , resetRRState
  ) where

import Control.Monad
import Control.Monad.State.Lazy as ST
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.IORef
#ifdef DHALL
import GHC.Natural (Natural)
#endif
import Network.Wai.Application.Classic
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import Program.Mighty.Parser

----------------------------------------------------------------

-- | A logical path specified in URL.
type Src      = Path
-- | A physical path in a file system.
type Dst      = Path
type Domain   = ByteString
#ifdef DHALL
type Port     = Natural
type Weight   = Natural
#else
type Port     = Int
type Weight   = Int
#endif

data Block    = Block [Domain] [Route] deriving (Eq,Show)
data Route    = RouteFile     Src Dst
              | RouteRedirect Src Dst
              | RouteCGI      Src Dst
              | RouteRevProxy Src Dst Domain Port Weight
              deriving (Eq,Show)
type RouteDB  = [Block]

----------------------------------------------------------------

-- | Parsing a route file.
parseRoute :: FilePath
           -> Domain -- ^ A default domain, typically \"localhost\"
           -> Port   -- ^ A default port, typically 80.
           -> Weight -- ^ A default weight for LoadBalancing (1)
           -> IO RouteDB
parseRoute file ddom dport dweight = parseFile (routeDB ddom dport dweight) file

routeDB :: Domain -> Port -> Weight -> Parser RouteDB
routeDB ddom dport dweight = commentLines *> many1 (block ddom dport dweight) <* eof

block :: Domain -> Port -> Weight -> Parser Block
block ddom dport dweight = Block <$> cdomains <*> many croute
  where
    cdomains = domains <* commentLines
    croute   = route ddom dport dweight  <* commentLines

domains :: Parser [Domain]
domains = open *> doms <* close <* trailing
  where
    open  = () <$ char '[' *> spcs
    close = () <$ char ']' *> spcs
    doms = (domain `sepBy1` sep) <* spcs
    domain = BS.pack <$> many1 (noneOf "[], \t\n")
    sep = () <$ spcs1

data Op = OpFile | OpCGI | OpRevProxy | OpRedirect

route :: Domain -> Port -> Weight -> Parser Route
route ddom dport dweight = do
    s <- src
    o <- op
    case o of
        OpFile     -> RouteFile     s <$> dst <* trailing
        OpRedirect -> RouteRedirect s <$> dst' <* trailing
        OpCGI      -> RouteCGI      s <$> dst <* trailing
        OpRevProxy -> do
            (dom,prt,d,wt) <- domPortDst ddom dport dweight
            return $ RouteRevProxy s d dom prt wt
  where
    src = path
    dst = path
    dst' = path'
    op0 = OpFile     <$ string "->"
      <|> OpRedirect <$ string "<<"
      <|> OpCGI      <$ string "=>"
      <|> OpRevProxy <$ string ">>"
    op  = op0 <* spcs

path :: Parser Path
path = do
    c <- char '/'
    BS.pack . (c:) <$> many (noneOf "[], \t\n, ;") <* spcs

path' :: Parser Path
path' = BS.pack <$> many (noneOf "[], \t\n") <* spcs

domPortDst :: Domain -> Port -> Weight -> Parser (Domain, Port, Dst, Weight)
domPortDst ddom dport dweight = (ddom,,,)    <$> port   <*> path <*> weight
                            <|> try((,,,)    <$> domain <*> port <*> path <*> weight)
                            <|> (,,,dweight) <$> domain <*> port <*> path
                            <|> (,dport,,)   <$> domain <*> path <*> weight
  where
    domain = BS.pack <$> many1 (noneOf ":/[], \t\n")
    port = do
        void $ char ':'
        read <$> many1 (oneOf ['0'..'9'])
    weight = do
        void $ char '-'
        read <$> many1 (oneOf ['0'..'9']) <* spcs

---------------------------------------------------------------

newtype ReqRef = ReqRef (IORef Int)

initRRState :: Int -> IO ReqRef
initRRState state = ReqRef <$> newIORef state

getRRState :: ReqRef -> IO Int
getRRState (ReqRef reqState) = readIORef reqState

setRRState :: ReqRef -> IO ()
setRRState (ReqRef reqState) = do
  modifyIORef reqState (+1)

resetRRState :: ReqRef -> IO ()
resetRRState (ReqRef reqState) = do
  writeIORef reqState 0

----------------------------------------------------------------

newtype RouteDBRef = RouteDBRef (IORef RouteDB)

newRouteDBRef :: RouteDB -> IO RouteDBRef
newRouteDBRef rout = RouteDBRef <$> newIORef rout

readRouteDBRef :: RouteDBRef -> IO RouteDB
readRouteDBRef (RouteDBRef ref) = readIORef ref

writeRouteDBRef :: RouteDBRef -> RouteDB -> IO ()
writeRouteDBRef (RouteDBRef ref) rout = writeIORef ref rout
