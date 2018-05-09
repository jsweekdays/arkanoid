port module Main exposing (..)

import AnimationFrame
import Collage exposing (..)
import Element exposing (..)
import Color
import Time exposing (Time)
import Text
import Html exposing (..)
import Keyboard exposing (KeyCode)
import Physics exposing (..)


---- CONSTANTS ----


interfaceHeight : Float
interfaceHeight =
    400


interfaceWidth : Float
interfaceWidth =
    800


spaceKeyCode : Int
spaceKeyCode =
    32


barHeight : Float
barHeight =
    20


barWidth : Float
barWidth =
    200


blockHeight : Float
blockHeight =
    20


blockWidth : Float
blockWidth =
    60


ballRadius : Float
ballRadius =
    30



---- MODEL ----


type Status
    = Start
    | Play
    | Failed
    | Complete


type alias Model =
    { ball : Body
    , bar : Body
    , blocks : List Body
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( { ball =
            { position = Position 0 (-ballRadius * 2)
            , velocity = Velocity 0 0.1
            , shape = Circle (CircleData ballRadius)
            }
      , bar =
            { position = Position 0 (-interfaceHeight / 2 + barHeight / 2)
            , velocity = Velocity 0 0
            , shape = Rectangle (RectangleData barHeight barWidth)
            }
      , blocks =
            [ { position = Position 0 ballRadius
              , velocity = Velocity 0 0
              , shape = Rectangle (RectangleData blockHeight blockWidth)
              }
            ]
      , status = Play
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time
    | KeyDown KeyCode



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                futureBall =
                    updatePositionByTime dt model.ball
            in
                ( { model
                    | ball = futureBall
                  }
                , Cmd.none
                )

        KeyDown code ->
            if code == spaceKeyCode then
                ( { model | status = Play }, Cmd.none )
            else
                ( model, Cmd.none )


---- VIEW ----


outlineStyle =
    { defaultLine | width = 2, color = Color.black }


background : Form
background =
    rect interfaceWidth interfaceHeight
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


drawCircle : Physics.Position -> CircleData -> Form
drawCircle { x, y } { radius } =
    group
        [ circle radius |> filled Color.red |> move ( x, y )
        , circle radius |> outlined outlineStyle |> move ( x, y )
        ]


drawRectangle : Physics.Position -> RectangleData -> Form
drawRectangle { x, y } { height, width } =
    group
        [ rect width height |> filled Color.grey |> move ( x, y )
        , rect width height |> outlined outlineStyle |> move ( x, y )
        ]

drawBody : Body -> Form
drawBody body =
  case body.shape of
    Circle data ->
      drawCircle body.position data

    Rectangle data ->
      drawRectangle body.position data


drawScene : List Form -> Html Msg
drawScene forms =
    let
        height =
            ceiling interfaceHeight

        width =
            ceiling interfaceWidth
    in
        collage width height forms |> toHtml


view : Model -> Html Msg
view model =
    case model.status of
        Play ->
            drawScene
                [ background
                , group <| List.map drawBody <| model.blocks
                , drawBody model.bar
                , drawBody model.ball
                ]

        _ ->
            drawScene
                [ background
                , drawStatus model.status
                ]



---- PORTS ----


port moveBar : (( Float, Float ) -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDown
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
