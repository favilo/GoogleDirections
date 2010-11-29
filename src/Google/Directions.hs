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
module Google.Directions (
        TravelMode (..),
        directions,
        Waypoints (..),
        Avoidable (..),
        Units (..),
        Directions (..),
        Route (..),
        Distance (..),
        Duration (..),
        Coord,
        Leg (..),
        Step (..),
        PolyLine (..),
        StatusCode (..),
    ) where

import Network.Curl.Download
import Text.JSON.AttoJSON as JSON
import Data.Ratio

import qualified Codec.Binary.Url as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M


data TravelMode = Driving 
    | Walking
    | Bicycling

instance Show TravelMode where
    show Driving = "driving"
    show Walking = "walking"
    show Bicycling = "bicycling"

data Waypoints = Waypoints Bool [B.ByteString]

instance Show Waypoints where
    show (Waypoints _ []) = ""
    show (Waypoints optimize points) = 
        "&waypoints="  
        ++ (if optimize then "optimize:true|"
                     else "")
        ++ showWaypoints points
            where
                showWaypoints [point] = C.encode (B.unpack point)
                showWaypoints (p:ps)  = C.encode (B.unpack p) ++ "|" ++ showWaypoints ps

data Avoidable = Tolls
    | Highways

instance Show Avoidable where
    show Tolls = "avoid=tolls"
    show Highways = "avoid=highways"

showAvoidables [] = ""
showAvoidables as = "&" ++ show' as
        where show' [a] = show a
              show' (a:as) = show a ++ showAvoidables as

data Units = Imperial
    | Metric

data Directions = Directions {
    status :: StatusCode,
    routes :: [Route]
} deriving (Show)

data Route = Route {
    summary :: B.ByteString,
    legs    :: [Leg],
    waypointOrder    :: [Integer],
    --overviewPolyline :: PolyLine,
    copyrights       :: B.ByteString,
    warnings         :: [B.ByteString]
} deriving (Show)

data Distance = Dist Double B.ByteString deriving (Show)
data Duration = Dur Double B.ByteString deriving (Show)
type Coord    = (Double, Double)

data Leg = Leg {
    steps    :: [Step],
    legDistance    :: Distance,
    legDuration    :: Duration,
    legStartLocation   :: Coord,
    legEndLocation     :: Coord,
    startAddress    :: B.ByteString,
    endAddress      :: B.ByteString
} deriving (Show)

data Step = Step {
    htmlInstructions    :: B.ByteString,
    stepDistance    :: Distance,
    stepDuration    :: Duration,
    stepStartLocation   :: Coord,
    stepEndLocation     :: Coord
} deriving (Show)

data PolyLine = PolyLine {
    points          :: B.ByteString,
    levels          :: B.ByteString
} deriving (Show)

data StatusCode = OK
    | NotFound
    | ZeroResults
    | MaxWaypointsExceeded
    | InvalidRequest
    | OverQueryLimit
    | RequestDenied
    | UnknownError
        deriving (Show)

directions :: 
       B.ByteString     -- The origin address
    -> B.ByteString     -- The destination address
    -> Maybe TravelMode -- The mode of transport to use
    -> Maybe Waypoints  -- Should google alter a route with waypoints
    -> Bool             -- Should google search for alternate routes
    -> [Avoidable]      -- Should google avoid tolls and/or highways
    -> Maybe Units      -- the unit system to use
    -> Bool             -- sensor
    -> IO (Either String Directions)
directions origin dest travelMode 
           waypoints alternate 
           avoidables units sensor = do
    let url = "http://maps.googleapis.com/maps/api/directions/json?"
              ++ ("origin=" ++ C.encode (B.unpack origin))
              ++ ("&destination=" ++ C.encode (B.unpack dest))
              ++ (case travelMode of
                    Nothing -> ""
                    Just mode -> "&mode=" ++ show mode
                )
              ++ (case waypoints of
                    Nothing -> ""
                    Just points -> (show points)
                 )
              ++ "&alternates=" ++ (case alternate of
                      True -> "true"
                      False -> "false"
                      )
              ++ (showAvoidables avoidables)
              ++ (case units of
                      Nothing -> ""
                      Just Imperial -> "&units=imperial"
                      Just Metric   -> "&units=metric"
                 )
              ++ "&sensor=" ++ (case sensor of
                      True -> "true"
                      False -> "false"
                      )
    string <- openURI url
    --putStrLn $ show string
    return $ parseDirections string

parseDirections (Left err) = Left err
parseDirections (Right json) = 
    case parseJSON json of
        Left err -> Left err
        Right jsvalue ->
            parseDirections' jsvalue

parseDirections' jsvalue = case JSON.lookup "status" jsvalue of
    Just (JSString str) -> Right $ Directions (parseStatus str) 
            (parseRoutes $ JSON.lookup "routes" jsvalue)
    _ -> Left "error, can't parse"

parseStatus "OK" = OK
parseStatus "NOT_FOUND" = NotFound
parseStatus "ZERO_RESULTS" = ZeroResults
parseStatus "MAX_WAYPOINTS_EXCEEDED" = MaxWaypointsExceeded
parseStatus "INVALID_REQUEST" = InvalidRequest
parseStatus "OVER_QUERY_LIMIT" = OverQueryLimit
parseStatus "REQUEST_DENIED" = RequestDenied
parseStatus _ = UnknownError

parseRoutes :: Maybe JSValue -> [Route]
parseRoutes routes = case routes of
    Just (JSArray jsRoutes) -> map parseRoute jsRoutes
    _ -> []

parseRoute route = Route 
    (case JSON.lookup "summary" route of
        Just (JSString sum) -> sum
        _ -> error "summary parse failed"
    ) 
    (case JSON.lookup "legs" route of
        Just (JSArray legs) -> map parseLeg legs
        _ -> error "legs parse failed"
    )
    (case JSON.lookup "waypoint_order" route of
        Just (JSArray warns) -> map 
            (\warn -> 
                (case warn of
                    JSNumber n -> numerator n
                    _ -> error "warning parse failed"
                )
            ) warns
        _ -> error "warnings parse failed"
    )
    (case JSON.lookup "copyrights" route of
        Just (JSString s) -> s
        _ -> error "copyrights parse failed"
    )
    (case JSON.lookup "warnings" route of
        Just (JSArray warns) -> map 
            (\warn -> 
                (case warn of
                    JSString s -> s
                    _ -> error "warning parse failed"
                )
            ) warns
        _ -> error "warnings parse failed"
    )

parseLeg :: JSValue -> Leg
parseLeg leg = Leg
    (case JSON.lookup "steps" leg of
        Just (JSArray stps) -> map parseStep stps
        _ -> error "step parse failed"
    )
    (case JSON.lookup "distance" leg of
        Just (JSObject m) -> parseDistance m
        _ -> error "distance parse failed"
    )
    (case JSON.lookup "duration" leg of
        Just (JSObject m) -> parseDuration m
        _ -> error "duration parse failed"
    )
    (case JSON.lookup "start_location" leg of
        Just (JSObject m) -> parseLocation m
        _ -> error "start_location parse failed"
    )
    (case JSON.lookup "end_location" leg of
        Just (JSObject m) -> parseLocation m
        _ -> error "end_location parse failed"
    )
    (case JSON.lookup "start_address" leg of
        Just (JSString string) -> string
        _ -> error "start_address parse failed"
    )
    (case JSON.lookup "end_address" leg of
        Just (JSString string) -> string
        _ -> error "end_address parse failed"
    )
    
parseStep :: JSValue -> Step
parseStep step = Step
    (case JSON.lookup "html_instructions" step of
        Just (JSString string) -> string
        _ -> error "html_instructions parse failed"
    )
    (case JSON.lookup "distance" step of
        Just (JSObject m) -> parseDistance m
        _ -> error "distance parse failed"
    )
    (case JSON.lookup "duration" step of
        Just (JSObject m) -> parseDuration m
        _ -> error "duration parse failed"
    )
    (case JSON.lookup "start_location" step of
        Just (JSObject m) -> parseLocation m
        _ -> error "start_location parse failed"
    )
    (case JSON.lookup "end_location" step of
        Just (JSObject m) -> parseLocation m
        _ -> error "end_location parse failed"
    )

parseDistance m = Dist
    (case m M.! "value" of
        JSNumber n -> fromRational n
        _ -> error "value parse failed"
    )
    (case m M.! "text" of
        JSString s -> s
        _ -> error "text parse failed"
    )
parseDuration m = Dur
    (case m M.! "value" of
        JSNumber n -> fromRational n
        _ -> error "value parse failed"
    )
    (case m M.! "text" of
        JSString s -> s
        _ -> error "text parse failed"
    )
parseLocation m = ((case m M.! "lat" of
            JSNumber n -> fromRational n),
        (case m M.! "lat" of
            JSNumber n -> fromRational n))
