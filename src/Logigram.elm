module Logigram exposing (..)

import Html
import Dict


main =
    Html.text "Hello, World"


type Family
    = Daan
    | Marlies
    | Sophie
    | Robin
    | Hannah


type Hat
    = Red
    | Orange
    | Yellow
    | Green
    | Blue


type alias Logigram =
    { current : Dict.Dict Family Hat
    , target : Dict.Dict Family Hat
    }
