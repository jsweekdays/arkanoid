port module Main exposing (..)

import AnimationFrame
import Collage exposing (..)
import Element exposing (..)
import Color
import Time exposing (Time)
import Text
import Html exposing (..)


---- CONSTANTS ----


interfaceHeight : Float
interfaceHeight =
    400


interfaceWidth : Float
interfaceWidth =
    800



---- MODEL ----


type Status
    = Start
    | Play
    | Failed
    | Complete


type alias Model =
    {
      status: Status
    }


init : ( Model, Cmd Msg )
init =
    ( {
      status = Start
    }, Cmd.none )



---- UPDATE ----


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


background : Form
background =
    rect interfaceHeight interfaceHeight
        |> filled (Color.rgb 240 240 240)


drawText : String -> Form
drawText =
    Text.fromString
        >> Text.height 14
        >> Text.monospace
        >> Collage.text


drawStatus : Status -> Form
drawStatus status =
    case status of
        Start ->
            drawText "Press space key to play"

        Play ->
            drawText "Game is on.."

        Failed ->
            drawText "You lose!"

        Complete ->
            drawText "You win!"

view : Model -> Html Msg
view model =
    let
        height =
            ceiling interfaceHeight

        width =
            ceiling interfaceWidth

        forms =
            [ background
            , drawStatus model.status
            ]
    in
        collage width height forms |> toHtml



---- PORTS ----


port moveBar : (( Float, Float ) -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
