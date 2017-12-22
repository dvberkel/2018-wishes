module Robot exposing (..)

import Html


main =
    Html.text "Hello, World!"


type alias Robot =
    { heading : Heading
    , position : Position
    }


type Heading
    = North
    | East
    | South
    | West


type alias Position =
    ( Int, Int )


type Program
    = Idle
    | Move
    | Left
    | Right
    | Sequence (List Program)
    | Repeat Int Program

