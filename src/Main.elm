port module Main exposing (..)

import AnimationFrame
import Collage exposing (..)
import Element exposing (..)
import Color
import Time exposing (Time)
import Text
import Html exposing (..)
import Keyboard exposing (KeyCode)


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


type alias Positioned a =
    { a | x : Float, y : Float }


type alias Blocked a =
    { a | width : Float, height : Float }


type alias Moved a =
    { a | vx : Float, vy : Float }


type alias Rounded a =
    { a | radius : Float }


type alias Ball =
    Moved (Rounded (Positioned {}))


type alias Bar =
    Moved (Blocked (Positioned {}))


type alias Block =
    Positioned (Blocked {})


type alias Boundary =
    { top : Float, bottom : Float, left : Float, right : Float }


type Direction
    = Horizontal
    | Vertical


type alias Collision =
    Maybe Direction


type alias Model =
    { ball : Ball
    , bar : Bar
    , blocks : List Block
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( { ball = { x = 0, y = -ballRadius * 2, radius = ballRadius, vx = 0, vy = 0.25 }
      , bar = { x = 0, y = -interfaceHeight / 2 + barHeight / 2, width = barWidth, height = barHeight, vx = 0, vy = 0 }
      , blocks =
            [ { x = 0, y = ballRadius * 2, width = blockWidth, height = blockHeight }
            ]
      , status = Play
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time
    | KeyDown KeyCode

extractBlockBoundary : Blocked (Positioned a) -> Boundary
extractBlockBoundary {x, y, width, height} =
    { left = x - width / 2
    , right = x + width / 2
    , top = y + height / 2
    , bottom = y - height / 2
    }


extractRoundBoundary : Rounded (Positioned a) -> Boundary
extractRoundBoundary {x, y, radius} =
    { left = x - radius / 2
    , right = x + radius / 2
    , top = y + radius / 2
    , bottom = y - radius / 2
    }

intersection : Boundary -> Boundary -> Bool
intersection b1 b2 =
    b1.left
        <= b2.right
        && b2.left
        <= b1.right
        && b1.bottom
        <= b2.top
        && b2.bottom
        <= b1.top


updatePositionByVelocity : Time -> Moved (Positioned a) -> Moved (Positioned a)
updatePositionByVelocity dt object =
    { object
        | x = object.x + object.vx * dt
        , y = object.y + object.vy * dt
    }


updateVelocityByCollision : Collision -> Moved a -> Moved a
updateVelocityByCollision collision object =
    case collision of
        Just Horizontal ->
            { object | vx = (-1) * object.vx }

        Just Vertical ->
            { object | vy = (-1) * object.vy }

        Nothing ->
            object


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | ball = updatePositionByVelocity dt model.ball
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


drawRounded : Rounded (Positioned a) -> Form
drawRounded { radius, x, y } =
    group
        [ circle radius |> filled Color.red |> move ( x, y )
        , circle radius |> outlined outlineStyle |> move ( x, y )
        ]


drawBlocked : Blocked (Positioned a) -> Form
drawBlocked { height, width, x, y } =
    group
        [ rect width height |> filled Color.grey |> move ( x, y )
        , rect width height |> outlined outlineStyle |> move ( x, y )
        ]


drawBlockedList : List (Blocked (Positioned a)) -> Form
drawBlockedList blocks =
    blocks
        |> List.map drawBlocked
        |> group


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
                , drawBlockedList model.blocks
                , drawBlocked model.bar
                , drawRounded model.ball
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
