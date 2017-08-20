module Main exposing (..)

import AnimationFrame
import Clock
import Color
import Constants
import Direction
import Element
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Job
import Price
import Resource
import Soldier
import Text
import Time exposing (Time)
import Villager exposing (Villager)


-- MODEL


type alias Position =
    ( Float, Float )


type alias Model =
    { villagers : List Villager
    , soldiers : List Soldier.Soldier
    , gold : Int
    , food : Int
    , houses : Int
    , clock : Clock.Clock
    }


init : ( Model, Cmd Msg )
init =
    ( { villagers = []
      , soldiers = []
      , gold = 1000 --10
      , food = 1000 --3
      , houses = 1

      -- Physics run 30 times per second (fixed time step)
      , clock = Clock.withPeriod (33 * Time.millisecond)
      }
    , Cmd.none
    )



-- UPDATE


type Structure
    = House


type Msg
    = NoOp
    | Tick Time
    | SpawnVillager Job.Job
    | Build Structure
    | TimeTravel Time


updateVillager : Float -> Villager -> ( Int, Int, Villager )
updateVillager dt villager =
    case villager.action of
        Villager.Farming ( curr, target ) ->
            if curr >= toFloat target then
                -- Done harvesting, so walk back to village with resource
                let
                    overflow =
                        curr - toFloat target
                in
                ( 0
                , 0
                , { villager
                    | action = Villager.Moving Direction.Right ( overflow, 0 )
                    , carrying = Just (Resource.Food target)
                  }
                )
            else
                -- Still harvesting resource
                ( 0
                , 0
                , { villager
                    | action = Villager.Farming ( curr + (dt * villager.farmingSpeed), target )
                  }
                )

        Villager.Mining ( curr, target ) ->
            if curr >= toFloat target then
                -- Done mining, so walk back to village with gold
                let
                    overflow =
                        curr - toFloat target
                in
                ( 0
                , 0
                , { villager
                    | action = Villager.Moving Direction.Right ( overflow, 0 )
                    , carrying = Just (Resource.Gold target)
                  }
                )
            else
                -- Still mining
                ( 0
                , 0
                , { villager
                    | action = Villager.Mining ( curr + (dt * villager.miningSpeed), target )
                  }
                )

        Villager.Moving direction ( x, y ) ->
            case direction of
                Direction.Left ->
                    if x <= 0 then
                        -- Reached the resource area, so enter it and start harvesting
                        let
                            overflow =
                                negate x

                            newAction =
                                case villager.job of
                                    Job.Miner ->
                                        Villager.Mining ( overflow, 1 )

                                    Job.Farmer ->
                                        Villager.Farming ( overflow, 1 )
                        in
                        ( 0
                        , 0
                        , { villager | action = newAction }
                        )
                    else
                        -- Still walking left
                        ( 0
                        , 0
                        , { villager | action = Villager.Moving Direction.Left ( x - (dt * villager.movingSpeed), y ) }
                        )

                Direction.Right ->
                    if x >= Constants.roadTileLength then
                        let
                            overflow =
                                x - Constants.roadTileLength

                            newVillager =
                                { villager
                                    | action = Villager.Moving Direction.Left ( Constants.roadTileLength - overflow, y )
                                    , carrying = Nothing
                                }
                        in
                        -- Hit village center, so turn in resource and turn around
                        case villager.carrying of
                            Nothing ->
                                ( 0, 0, newVillager )

                            Just (Resource.Gold count) ->
                                ( count, 0, newVillager )

                            Just (Resource.Food count) ->
                                ( 0, count, newVillager )
                    else
                        -- Still walking right
                        ( 0
                        , 0
                        , { villager | action = Villager.Moving Direction.Right ( x + (dt * villager.movingSpeed), y ) }
                        )


{-| Subtracts price from model resources
-}
applyPrice : Price.Price -> Model -> Model
applyPrice { food, gold } model =
    { model
        | food = model.food - food
        , gold = model.gold - gold
    }


canAfford : Price.Price -> Model -> Bool
canAfford { food, gold } model =
    model.food >= food && model.gold >= gold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Build structure ->
            let
                price =
                    case structure of
                        House ->
                            Price.priceOfHouse model.houses

                newModel =
                    case structure of
                        House ->
                            { model
                                | houses = model.houses + 1
                            }
            in
            ( applyPrice price newModel
            , Cmd.none
            )

        TimeTravel diff ->
            update (Tick diff) model

        SpawnVillager job ->
            let
                price =
                    Price.priceOfVillager (List.length model.villagers)

                villager =
                    { action = Villager.Moving Direction.Left ( Constants.roadTileLength, 0 )
                    , carrying = Nothing
                    , job = job
                    , movingSpeed = 2.0
                    , miningSpeed = 0.2
                    , farmingSpeed = 0.2
                    }
            in
            ( { model
                | villagers = villager :: model.villagers
              }
                |> applyPrice price
            , Cmd.none
            )

        Tick diff ->
            let
                ( newClock, newModel ) =
                    Clock.update stepPhysics diff model.clock model
            in
            ( { newModel
                | clock = newClock
              }
            , Cmd.none
            )


stepPhysics : Time -> Model -> Model
stepPhysics diff model =
    let
        dt =
            Time.inSeconds diff

        ( deltaGold, deltaFood, newVillagers ) =
            List.foldl
                (\villager ( gold, food, villagers ) ->
                    let
                        ( producedGold, producedFood, newVillager ) =
                            updateVillager dt villager
                    in
                    ( gold + producedGold
                    , food + producedFood
                    , newVillager :: villagers
                    )
                )
                ( 0, 0, [] )
                model.villagers

        newModel =
            { model
                | villagers = newVillagers
                , gold = model.gold + deltaGold
                , food = model.food + deltaFood
            }
    in
    newModel



-- VIEW


viewVillager : Villager -> Element.Element
viewVillager villager =
    let
        pxWidth =
            round Constants.villagerSize

        pxHeight =
            round Constants.villagerSize

        base =
            Element.empty
                |> Element.width pxWidth
                |> Element.height pxHeight
                |> Element.color Color.black

        resource =
            Element.container
                pxWidth
                pxHeight
                Element.midTop
                (case villager.carrying of
                    Nothing ->
                        Element.empty

                    Just (Resource.Gold _) ->
                        Element.empty
                            |> Element.width (round <| Constants.villagerSize / 2)
                            |> Element.height (round <| Constants.villagerSize / 2)
                            |> Element.color Color.yellow

                    Just (Resource.Food _) ->
                        Element.empty
                            |> Element.width (round <| Constants.villagerSize / 2)
                            |> Element.height (round <| Constants.villagerSize / 2)
                            |> Element.color Color.lightRed
                )
    in
    Element.layers [ base, resource ]


viewResourceSite : Element.Element -> Element.Element
viewResourceSite =
    Element.container 125 20 Element.midRight


viewFarm : List Villager -> Element.Element
viewFarm villagers =
    let
        occupants =
            List.length <| List.filter Villager.isFarming villagers

        jobs =
            List.length <| List.filter Villager.isFarmer villagers
    in
    (Text.fromString <| "[Farm " ++ toString occupants ++ "/" ++ toString jobs ++ "]")
        |> Element.rightAligned


viewGoldMine : List Villager -> Element.Element
viewGoldMine villagers =
    let
        occupants =
            List.length <| List.filter Villager.isMining villagers

        jobs =
            List.length <| List.filter Villager.isMiner villagers
    in
    (Text.fromString <| "[Mine " ++ toString occupants ++ "/" ++ toString jobs ++ "]")
        |> Element.centered


viewRoad : List Villager -> Element.Element
viewRoad villagers =
    let
        roadPixelWidth =
            round <| Constants.roadTileLength * Constants.tileSize + Constants.tileSize

        roadPixelHeight =
            round <| Constants.tileSize * 2

        -- Returns y-axis offset jitter depending on x (will be >= 0)
        jitter x =
            if x % 2 == 0 then
                0
            else
                1
    in
    Element.layers <|
        List.concat
            [ [ Element.empty
                    |> Element.width roadPixelWidth
                    |> Element.height roadPixelHeight
              ]
            , List.map
                (\villager ->
                    case villager.action of
                        Villager.Moving direction ( x, y ) ->
                            let
                                pxFromLeft =
                                    round <| Constants.tileSize * x

                                pxFromTop =
                                    -- Villagers going left are in top lane
                                    -- Villagers goiing right are in bottom lane
                                    jitter (round x)
                                        + (case direction of
                                            Direction.Left ->
                                                0

                                            Direction.Right ->
                                                round Constants.tileSize
                                          )

                                position =
                                    Element.topLeftAt (Element.absolute pxFromLeft) (Element.absolute pxFromTop)
                            in
                            Element.container roadPixelWidth roadPixelHeight position (viewVillager villager)

                        _ ->
                            Element.empty
                )
                villagers
            ]


viewVillage : Element.Element
viewVillage =
    Text.fromString "[Village]"
        |> Element.centered


viewPrice : Price.Price -> String
viewPrice { gold, food } =
    case ( gold, food ) of
        ( 0, 0 ) ->
            "(Free)"

        ( _, 0 ) ->
            "(" ++ toString gold ++ " gold)"

        ( 0, _ ) ->
            "(" ++ toString food ++ " food)"

        ( _, _ ) ->
            "(" ++ toString gold ++ " gold, " ++ toString food ++ " food)"


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.class "container grid-1280" ]
        [ Html.text <| "Gold: " ++ toString model.gold
        , Html.text " "
        , Html.text <| "Food: " ++ toString model.food
        , Html.text " "
        , Html.text <| "Population: " ++ toString (List.length model.villagers) ++ "/" ++ toString (model.houses * 3)
        , Element.flow Element.down
            [ Element.flow Element.right
                [ viewResourceSite <| viewFarm model.villagers
                , viewRoad (List.filter Villager.isFarmer model.villagers)
                , viewVillage
                ]
                |> Element.color Color.lightBlue
            , Element.flow Element.right
                [ viewResourceSite <| viewGoldMine model.villagers
                , viewRoad (List.filter Villager.isMiner model.villagers)
                , viewVillage
                ]
                |> Element.color Color.lightPurple
            ]
            |> Element.toHtml

        -- BUILD
        , Html.div
            []
            [ Html.text "Build: "
            , let
                price =
                    Price.priceOfHouse model.houses

                unaffordable =
                    not (canAfford price model)
              in
              Html.button
                [ Html.Events.onClick (Build House)
                , Html.Attributes.disabled unaffordable
                ]
                [ Html.text <| "House " ++ viewPrice price
                ]
            ]

        -- SPAWN
        , let
            overpopulated =
                List.length model.villagers >= model.houses * 3

            price =
                Price.priceOfVillager (List.length model.villagers)

            unaffordable =
                not (canAfford price model)

            disabled =
                overpopulated || unaffordable
          in
          Html.div
            []
            [ Html.span
                []
                [ Html.text "Spawn: " ]
            , Html.button
                [ Html.Events.onClick (SpawnVillager Job.Farmer)
                , Html.Attributes.disabled disabled
                ]
                [ Html.text <| "Farmer " ++ viewPrice price ]
            , Html.button
                [ Html.Events.onClick (SpawnVillager Job.Miner)
                , Html.Attributes.disabled disabled
                ]
                [ Html.text <| "Miner " ++ viewPrice price ]
            ]
        , Html.div
            []
            [ Html.text "AddSeconds: "
            , Html.button
                [ Html.Events.onClick (TimeTravel Time.second) ]
                [ Html.text "+1" ]
            , Html.button
                [ Html.Events.onClick (TimeTravel (Time.second * 10)) ]
                [ Html.text "+10" ]
            , Html.button
                [ Html.Events.onClick (TimeTravel (Time.second * 100)) ]
                [ Html.text "+100" ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
