module Physics exposing (..)

import Time exposing (Time)


type alias Position =
    { x : Float, y : Float }


type alias Velocity =
    { vx : Float, vy : Float }


type alias CircleData =
    { radius : Float }


type alias RectangleData =
    { height : Float, width : Float }


type Shape =
    Circle CircleData |
    Rectangle RectangleData


type alias Body =
    { position : Position
    , shape : Shape
    , velocity : Velocity
    }


type alias Boundary =
    { top : Float, bottom : Float, left : Float, right : Float }


type Direction
    = Horizontal
    | Vertical


type Side
    = Top
    | Right
    | Bottom
    | Left


type alias Collision =
    Maybe Direction

calculateCircleBoundary : Position -> CircleData -> Boundary
calculateCircleBoundary { x, y } { radius } =
  { left = x - radius / 2
  , right = x + radius / 2
  , top = y + radius / 2
  , bottom = y - radius / 2
  }

calculateReactangleBoundary : Position -> RectangleData -> Boundary
calculateReactangleBoundary { x, y } { height, width } =
  { left = x - width / 2
  , right = x + width / 2
  , top = y + height / 2
  , bottom = y - height / 2
  }

extractBoundary : Body -> Boundary
extractBoundary { shape, position } =
  let
    { x, y } =
        position
  in
    case shape of
      Circle form -> calculateCircleBoundary position form
      Rectangle form -> calculateReactangleBoundary position form


updatePositionByTime : Time -> Body -> Body
updatePositionByTime dt body =
    let
        { x, y } =
            body.position

        { vx, vy } =
            body.velocity
    in
        { body | position = { x = x + vx * dt, y = y + vy * dt } }


updateVelocityByCollision : Collision -> Body -> Body
updateVelocityByCollision collision body =
    let
        { vx, vy } =
            body.velocity
    in
        case collision of
            Just Horizontal ->
                { body | velocity = { vy = vy, vx = (-1) * vx } }

            Just Vertical ->
                { body | velocity = { vx = vx, vy = (-1) * vy } }

            Nothing ->
                body


intersection : Boundary -> Boundary -> Bool
intersection a b =
    a.bottom
        <= b.top
        && b.bottom
        <= a.top
        && a.left
        <= b.right
        && b.left
        <= a.right
