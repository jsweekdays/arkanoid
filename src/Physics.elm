module Physics exposing (..)

import Time exposing (Time)

type alias Positioned a =
    { a | x : Float, y : Float }


type alias Boxed a =
    { a | width : Float, height : Float }


type alias Moved a =
    { a | vx : Float, vy : Float }


type alias Rounded a =
    { a | radius : Float }


type alias Boundary =
    { top : Float, bottom : Float, left : Float, right : Float }


type Direction
    = Horizontal
    | Vertical


type alias Collision =
    Maybe Direction


extractBlockBoundary : Boxed (Positioned a) -> Boundary
extractBlockBoundary { x, y, width, height } =
    { left = x - width / 2
    , right = x + width / 2
    , top = y + height / 2
    , bottom = y - height / 2
    }


extractRoundBoundary : Rounded (Positioned a) -> Boundary
extractRoundBoundary { x, y, radius } =
    { left = x - radius / 2
    , right = x + radius / 2
    , top = y + radius / 2
    , bottom = y - radius / 2
    }



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


intersection : ( Boundary, Boundary ) -> Bool
intersection b =
    vIntersection b && hIntersection b


vIntersection : ( Boundary, Boundary ) -> Bool
vIntersection ( a, b ) =
    a.bottom <= b.top && b.bottom <= a.top


hIntersection : ( Boundary, Boundary ) -> Bool
hIntersection ( a, b ) =
    a.left <= b.right && b.left <= a.right
