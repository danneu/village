module Job exposing (..)


type Job
    = Miner
    | Farmer
    | Cadet ( Float, Float )
    | Soldier
