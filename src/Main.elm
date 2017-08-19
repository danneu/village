module Main exposing (..)

import AnimationFrame
import Collage
import Color
import Constants
import Direction
import Element
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Job
import Resource
import Text
import Time exposing (Time)
import Villager exposing (Villager)


-- MODEL


type alias Position =
    ( Float, Float )


type alias Model =
    { villagers : List Villager
    , gold : Int
    , food : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { villagers = []
      , gold = 0
      , food = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Tick Time
    | SpawnVillager Job.Job


updateVillager : Float -> Villager -> ( Int, Int, Villager )
updateVillager dt villager =
    case villager.action of
        Villager.Farming ( curr, target ) ->
            let
                newCurr =
                    curr + (dt * villager.farmingSpeed)
            in
            if curr >= toFloat target then
                -- Done harvesting, so walk back to village with resource
                ( 0
                , 0
                , { villager
                    | action = Villager.Moving Direction.Right ( 0, 0 )
                    , carrying = Just (Resource.Food target)
                  }
                )
            else
                -- Still harvesting resource
                ( 0
                , 0
                , { villager
                    | action = Villager.Farming ( newCurr, target )
                  }
                )

        Villager.Mining ( curr, target ) ->
            let
                newCurr =
                    curr + (dt * villager.miningSpeed)
            in
            if curr >= toFloat target then
                -- Done mining, so walk back to village with gold
                ( 0
                , 0
                , { villager
                    | action = Villager.Moving Direction.Right ( 0, 0 )
                    , carrying = Just (Resource.Gold target)
                  }
                )
            else
                -- Still mining
                ( 0
                , 0
                , { villager
                    | action = Villager.Mining ( newCurr, target )
                  }
                )

        Villager.Moving direction ( x, y ) ->
            let
                ( movedX, movedY ) =
                    case direction of
                        Direction.Left ->
                            ( x - (dt * villager.movingSpeed), y )

                        Direction.Right ->
                            ( x + (dt * villager.movingSpeed), y )
            in
            case direction of
                Direction.Left ->
                    if movedX <= 0 then
                        -- Reached the resource area, so enter it and start harvesting
                        let
                            newAction =
                                case villager.job of
                                    Job.Miner ->
                                        Villager.Mining ( 0, 1 )

                                    Job.Farmer ->
                                        Villager.Farming ( 0, 1 )
                        in
                        ( 0
                        , 0
                        , { villager | action = newAction }
                        )
                    else
                        -- Still walking left
                        ( 0
                        , 0
                        , { villager | action = Villager.Moving Direction.Left ( movedX, movedY ) }
                        )

                Direction.Right ->
                    if movedX >= Constants.roadTileLength then
                        let
                            newVillager =
                                { villager
                                    | action = Villager.Moving Direction.Left ( Constants.roadTileLength, movedY )
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
                        , { villager | action = Villager.Moving Direction.Right ( movedX, movedY ) }
                        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SpawnVillager job ->
            let
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
            , Cmd.none
            )

        Tick diff ->
            let
                dt =
                    Time.inSeconds diff

                _ =
                    if dt > 1 then
                        Debug.log "==================== dt" dt
                    else
                        dt

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
            ( newModel, Cmd.none )



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
                                    case direction of
                                        Direction.Left ->
                                            0

                                        Direction.Right ->
                                            round Constants.tileSize

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


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.class "container grid-1280" ]
        [ Html.text <| "Gold: " ++ toString model.gold
        , Html.text " "
        , Html.text <| "Food: " ++ toString model.food
        , Element.flow Element.down
            [ Element.flow Element.right
                [ viewResourceSite <| viewFarm model.villagers
                , viewRoad (List.filter Villager.isFarmer model.villagers)
                , viewVillage
                ]
            , Element.flow Element.right
                [ viewResourceSite <| viewGoldMine model.villagers
                , viewRoad (List.filter Villager.isMiner model.villagers)
                , viewVillage
                ]
            ]
            |> Element.toHtml
        , Html.span
            []
            [ Html.text "Spawn: " ]
        , Html.button
            [ Html.Events.onClick (SpawnVillager Job.Farmer) ]
            [ Html.text "Farmer " ]
        , Html.button
            [ Html.Events.onClick (SpawnVillager Job.Miner) ]
            [ Html.text "Miner" ]

        --, Html.p [] [ Html.text <| toString model.villagers ]
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
