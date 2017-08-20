module Price exposing (..)

{-| Returns price in gold.
-}


type alias Price =
    { gold : Int
    , food : Int
    }


priceOfHouse : Int -> Price
priceOfHouse count =
    let
        baseCost =
            10

        multiplier =
            1.1
    in
    { gold = round <| baseCost * (multiplier ^ toFloat count)
    , food = 0
    }


priceOfVillager : Int -> Price
priceOfVillager count =
    let
        baseCost =
            3

        multiplier =
            1.1
    in
    { food = round <| baseCost * (multiplier ^ toFloat count)
    , gold = 0
    }
