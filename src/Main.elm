port module Main exposing (..)

import AnimationFrame
import Html exposing (Html)
import Json.Encode as JE
import Time exposing (Time)


-- PORT


port draw : JE.Value -> Cmd msg



-- MODEL


type Resource
    = -- Int is quanity carrying
      Gold Int


type alias Position =
    ( Float, Float )


type Direction
    = Left
    | Right


type VillagerAction
    = -- (x, y)
      Moving Direction ( Float, Float )
      -- (progress, target) e.g. (50, 100) mining is halfway done
    | Mining ( Float, Int )


type alias Villager =
    { carrying : Maybe Resource
    , action : VillagerAction

    -- Tiles per second
    , movingSpeed : Float

    -- How much gold to mine per second
    , miningSpeed : Float
    }


type alias Model =
    { villagers : List Villager
    }


encodeVillager : Villager -> JE.Value
encodeVillager villager =
    JE.object
        [ ( "action"
          , case villager.action of
                Moving direction ( x, y ) ->
                    JE.object
                        [ ( "kind", JE.string "Moving" )
                        , ( "position", JE.list [ JE.int (round x), JE.int (round y) ] )
                        ]

                Mining _ ->
                    JE.object
                        [ ( "kind", JE.string "Mining" ) ]
          )
        , ( "carrying"
          , case villager.carrying of
                Nothing ->
                    JE.null

                Just resource ->
                    JE.string <| toString resource
          )
        ]


encodeModel : Model -> JE.Value
encodeModel model =
    JE.object
        [ ( "villagers", JE.list (List.map encodeVillager model.villagers) )
        ]


init : ( Model, Cmd Msg )
init =
    let
        initModel =
            { villagers =
                [ { action = Moving Left ( 10, 0 )
                  , carrying = Nothing
                  , movingSpeed = 2.0
                  , miningSpeed = 0.2
                  }
                ]
            }
    in
    ( initModel
    , draw (encodeModel initModel)
    )



-- UPDATE


type Msg
    = NoOp
    | Tick Time


updateVillager : Float -> Villager -> Villager
updateVillager dt villager =
    case villager.action of
        Mining ( curr, target ) ->
            let
                newCurr =
                    curr + (dt * villager.miningSpeed)
            in
            if curr >= toFloat target then
                -- Done mining, so walk back to village with gold
                { villager
                    | action = Moving Right ( 0, 0 )
                    , carrying = Just (Gold target)
                }
            else
                -- Still mining
                { villager
                    | action = Mining ( newCurr, target )
                }

        Moving direction ( x, y ) ->
            let
                ( movedX, movedY ) =
                    case direction of
                        Left ->
                            ( x - (dt * villager.movingSpeed), y )

                        Right ->
                            ( x + (dt * villager.movingSpeed), y )

                newAction =
                    case direction of
                        Left ->
                            if movedX <= 0 then
                                -- Hit left wall, so enter gold mine
                                Mining ( 0, 1 )
                            else
                                Moving Left ( movedX, movedY )

                        Right ->
                            if movedX >= 100 then
                                -- Hit right wall, so just turn around
                                Moving Left ( 100, movedY )
                            else
                                Moving Right ( movedX, movedY )
            in
            { villager
                | action = newAction
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick diff ->
            let
                dt =
                    Time.inSeconds diff

                newModel =
                    { model
                        | villagers = List.map (updateVillager dt) model.villagers
                    }
            in
            ( newModel, draw (encodeModel newModel) )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.text "Hello world"
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
