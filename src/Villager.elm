module Villager exposing (..)

import Direction exposing (Direction)
import Job
import Json.Encode as JE
import Resource exposing (Resource)


type Job
    = Miner
    | Farmer


type VillagerAction
    = -- (x, y)
      Moving Direction ( Float, Float )
      -- (progress, target) e.g. (50, 100) mining is halfway done
    | Mining ( Float, Int )
    | Farming ( Float, Int )


type alias Villager =
    { carrying : Maybe Resource
    , action : VillagerAction
    , job : Job.Job

    -- Tiles per second
    , movingSpeed : Float

    -- How much gold to mine per second
    , miningSpeed : Float

    -- How much food to farm per second
    , farmingSpeed : Float
    }


isMining : Villager -> Bool
isMining { action } =
    case action of
        Mining _ ->
            True

        _ ->
            False


isMiner : Villager -> Bool
isMiner { job } =
    case job of
        Job.Miner ->
            True

        _ ->
            False


isFarming : Villager -> Bool
isFarming { action } =
    case action of
        Farming _ ->
            True

        _ ->
            False


isFarmer : Villager -> Bool
isFarmer { job } =
    case job of
        Job.Farmer ->
            True

        _ ->
            False
