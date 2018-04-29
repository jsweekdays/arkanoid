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



type alias Ball =
    Moved (Rounded (Positioned {}))


type alias Bar =
    Moved (Boxed (Positioned {}))


type alias Block =
    Boxed (Positioned {})





type alias Model =
    { ball : Ball
    , bar : Bar
    , blocks : List Block
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( { ball = { x = 0, y = -ballRadius * 2, radius = ballRadius, vx = 0, vy = 0.1 }
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


findCollisionWithBlock : Rounded (Positioned a) -> Block -> Collision
findCollisionWithBlock ball block =
    let
        boundaries =
            ( extractRoundBoundary ball, extractBlockBoundary block )
    in
        if intersection boundaries then
            if hIntersection boundaries then
                Just Vertical
            else if vIntersection boundaries then
                Just Horizontal
            else
                Nothing
        else
            Nothing


findCollisionWithBar ball bar =
    let
        boundaries =
            ( extractRoundBoundary ball, extractBlockBoundary bar )
    in
        if intersection boundaries then
            if hIntersection boundaries then
                Just Vertical
            else if vIntersection boundaries then
                Just Horizontal
            else
                Nothing
        else
            Nothing


findCollisionWithBlocks : Rounded (Positioned a) -> List Block -> Collision
findCollisionWithBlocks ball blocks =
    let
        firstCollision =
            blocks
                |> List.map (\block -> findCollisionWithBlock ball block)
                |> List.filter (\collision -> collision /= Nothing)
                |> List.head
    in
        case firstCollision of
            Just collision ->
                collision
            _ ->
                Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                futureBall =
                    updatePositionByVelocity dt model.ball

                blockCollsion =
                    findCollisionWithBlocks futureBall model.blocks

                barCollsion =
                    findCollisionWithBar futureBall model.bar
            in
                ( { model
                    | ball =
                        futureBall
                            |> updateVelocityByCollision barCollsion
                            |> updateVelocityByCollision blockCollsion
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


drawBoxed : Boxed (Positioned a) -> Form
drawBoxed { height, width, x, y } =
    group
        [ rect width height |> filled Color.grey |> move ( x, y )
        , rect width height |> outlined outlineStyle |> move ( x, y )
        ]


drawBoxedList : List (Boxed (Positioned a)) -> Form
drawBoxedList blocks =
    blocks
        |> List.map drawBoxed
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
                , drawBoxedList model.blocks
                , drawBoxed model.bar
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
