module Soldier exposing (..)


type alias Stats =
    { hp : ( Float, Float )
    , damage : Float

    -- The second float is how many seconds must elapse before they can attack again.
    , speed : ( Float, Float )
    }


type SoldierAction
    = Moving
    | Fighting


type SoldierKind
    = Villager
    | Footman


type alias Soldier =
    { x : Int
    , kind : SoldierKind
    , stats : Stats
    , action : SoldierAction
    }


initStats : SoldierKind -> Stats
initStats kind =
    case kind of
        Villager ->
            { hp = ( 15, 15 )
            , damage = 3
            , speed = ( 0, 1 )
            }

        Footman ->
            { hp = ( 25, 25 )
            , damage = 5
            , speed = ( 0, 0.75 )
            }


init : SoldierKind -> Soldier
init kind =
    { x = 0
    , kind = kind
    , stats = initStats kind
    , action = Moving
    }
