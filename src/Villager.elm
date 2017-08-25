module Villager exposing (..)

import Direction exposing (Direction)
import Job
import Json.Encode as JE
import Resource exposing (Resource)


type VillagerAction
    = -- (x, y)
      Moving Direction ( Float, Float )
      -- (progress, target) e.g. (50, 100) mining is halfway done
    | Mining ( Float, Int )
    | Farming ( Float, Int )
    | Training ( Float, Int )
    | Crusading


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
    , trainingSpeed : Float

    --, xp : ( Float, Float )
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


isCadet : Villager -> Bool
isCadet { job } =
    case job of
        Job.Cadet _ ->
            True

        _ ->
            False


isSoldier : Villager -> Bool
isSoldier { job } =
    case job of
        Job.Soldier ->
            True

        _ ->
            False


isTraining : Villager -> Bool
isTraining { action } =
    case action of
        Training _ ->
            True

        _ ->
            False



-- JSON


encodeAction : VillagerAction -> JE.Value
encodeAction action =
    case action of
        Training ( curr, target ) ->
            JE.list
                [ JE.string "TRAINING"
                , JE.float curr
                , JE.int target
                ]

        _ ->
            JE.string "--"



-- type Job
--     = Miner
--     | Farmer
--     | Cadet ( Float, Float )


encodeJob : Job.Job -> JE.Value
encodeJob job =
    case job of
        Job.Miner ->
            JE.string "MINING"

        Job.Farmer ->
            JE.string "FARMER"

        Job.Cadet ( currXp, maxXp ) ->
            JE.list
                [ JE.string "CADET"
                , JE.float currXp
                , JE.float maxXp
                ]

        _ ->
            JE.string (toString job)


encode : Villager -> JE.Value
encode villager =
    JE.object
        [ ( "action", encodeAction villager.action )
        , ( "job", encodeJob villager.job )
        ]
