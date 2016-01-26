-- Author:     Favil Orbedios <favilo@gmail.com>
-- Maintainer: Favil Orbedios <favilo@gmail.com>
--
-- Copyright (C) 2010 Favil Orbedios, all rights reserved.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Google.Directions
       ( TravelMode (..)
       , directions
       , TargetTime (..)
       , Waypoints (..)
       , Avoidable (..)
       , Units (..)
       , Directions (..)
       , Route (..)
       , Distance (..)
       , Duration (..)
       , Coord (..)
       , Leg (..)
       , Step (..)
       , PolyLine (..)
       , StatusCode (..)
       ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Network.Curl.Download
import Network.HTTP.Base
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import qualified Data.List             as L
import qualified Data.Text             as T

data TravelMode
  = Driving
  | Walking
  | Bicycling
  | Transit

instance Show TravelMode where
  show Driving   = "driving"
  show Walking   = "walking"
  show Bicycling = "bicycling"
  show Transit   = "transit"

data TargetTime
  = ArrivalTime   UTCTime
  | DepartureTime UTCTime

data Waypoints = Waypoints Bool [Text]

instance Show Waypoints where
  show (Waypoints _     []) = ""
  show (Waypoints isOpt ps) = concat . L.intersperse "|" $ opt ++ map T.unpack ps
    where opt = if isOpt then ["optimize:true|"] else []

data Avoidable
  = Tolls
  | Highways

instance Show Avoidable where
  show Tolls    = "tolls"
  show Highways = "highways"

data Units
  = Imperial
  | Metric

instance Show Units where
  show Imperial = "imperial"
  show Metric   = "metric"

data Directions
  = Directions
  { status :: StatusCode
  , routes :: [Route]
  } deriving (Show)

data Route
  = Route
  { summary          :: Text
  , legs             :: [Leg]
  , waypointOrder    :: [Integer]
  --, overviewPolyline :: PolyLine
  , copyrights       :: Text
  , warnings         :: [Text]
  } deriving (Show)

data Distance = Dist Double Text deriving (Show)
data Duration = Dur  Double Text deriving (Show)
data Coord    = Coord {lat :: Double, lng:: Double} deriving (Show)

data Leg
  = Leg
  { steps            :: [Step]
  , legDistance      :: Distance
  , legDuration      :: Duration
  , legStartLocation :: Coord
  , legEndLocation   :: Coord
  , startAddress     :: Text
  , endAddress       :: Text
  } deriving (Show)

data Step
  = Step
  { htmlInstructions  :: Text
  , stepDistance      :: Distance
  , stepDuration      :: Duration
  , stepStartLocation :: Coord
  , stepEndLocation   :: Coord
  } deriving (Show)

data PolyLine
  = PolyLine
  { points :: Text
  , levels :: Text
  } deriving (Show)

data StatusCode
  = OK
  | NotFound
  | ZeroResults
  | MaxWaypointsExceeded
  | InvalidRequest
  | OverQueryLimit
  | RequestDenied
  | UnknownError
  deriving (Show)

directions :: Text             -- The origin address
           -> Text             -- The destination address
           -> Maybe TravelMode -- The mode of transport to use
           -> Maybe TargetTime -- The time of departure or arrival in POSIX time
           -> Maybe Waypoints  -- Should google alter a route with waypoints
           -> Bool             -- Should google search for alternate routes
           -> [Avoidable]      -- Should google avoid tolls and/or highways
           -> Maybe Units      -- the unit system to use
           -> Bool             -- sensor
           -> IO (Either String Directions)
directions origin dest travelMode targetTime waypoints alternate avoidables units sensor = do
  let url   = "https://maps.googleapis.com/maps/api/directions/json?"
      parms = urlEncodeVars $
              [ ("origin"     , T.unpack origin)
              , ("destination", T.unpack dest)
              , ("alternates" , if alternate then "true" else "false")
              , ("sensor"     , if sensor then "true" else "false")
              ]
              ++ (map (\x -> ("avoid", show x)) avoidables)
              ++ (maybe [] (\t -> [getTagertTimeParmeter t]) targetTime)
              ++ (maybe [] (\u -> [("units", show u)]) units)
              ++ (maybe [] (\m -> [("mode", show m)]) travelMode)
              ++ (maybe [] (\u -> [("waypoints", show u)]) waypoints)
  string <- openURI $ url ++ parms
  return $ parseDirections string

getTagertTimeParmeter :: TargetTime -> (String, String)
getTagertTimeParmeter x = case x of
  ArrivalTime   t -> ("arrival_time"  , show $ toSec t)
  DepartureTime t -> ("departure_time", show $ toSec t)
  where
    toSec :: UTCTime -> Integer
    toSec = round . utcTimeToPOSIXSeconds

parseDirections :: Either String B.ByteString -> Either String Directions
parseDirections (Left  e) = Left e
parseDirections (Right j) = case eitherDecode' $ BL.fromStrict j of
  Left err      -> Left err
  Right jsvalue -> Right jsvalue

instance FromJSON Directions where
  parseJSON (Object v) = Directions
    <$> v .: "status"
    <*> v .: "routes"
  parseJSON _ = fail "Not a direction"

instance FromJSON StatusCode where
  parseJSON (String v) = pure $ case v of
   "OK"                     -> OK
   "NOT_FOUND"              -> NotFound
   "ZERO_RESULTS"           -> ZeroResults
   "MAX_WAYPOINTS_EXCEEDED" -> MaxWaypointsExceeded
   "INVALID_REQUEST"        -> InvalidRequest
   "OVER_QUERY_LIMIT"       -> OverQueryLimit
   "REQUEST_DENIED"         -> RequestDenied
   _                        -> UnknownError
  parseJSON _ = fail "Not a direction"

instance FromJSON Route where
  parseJSON (Object v) = Route
    <$> v .: "summary"
    <*> v .: "legs"
    <*> v .: "waypoint_order"
    <*> v .: "copyrights"
    <*> v .: "warnings"
  parseJSON _ = mzero

instance FromJSON Leg where
  parseJSON (Object v) = Leg
    <$> v .: "steps"
    <*> v .: "distance"
    <*> v .: "duration"
    <*> v .: "start_location"
    <*> v .: "end_location"
    <*> v .: "start_address"
    <*> v .: "end_address"
  parseJSON _ = mzero

instance FromJSON Step where
  parseJSON (Object v) = Step
    <$> v .: "html_instructions"
    <*> v .: "distance"
    <*> v .: "duration"
    <*> v .: "start_location"
    <*> v .: "end_location"
  parseJSON _ = mzero

instance FromJSON Distance where
  parseJSON (Object v) = Dist
    <$> v .: "value"
    <*> v .: "text"
  parseJSON _ = mzero

instance FromJSON Duration where
  parseJSON (Object v) = Dur
    <$> v .: "value"
    <*> v .: "text"
  parseJSON _ = mzero

instance FromJSON Coord where
  parseJSON (Object v) = Coord
    <$> v .: "lat"
    <*> v .: "lng"
  parseJSON _ = mzero
