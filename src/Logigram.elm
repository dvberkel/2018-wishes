module Logigram exposing (..)

import Html
import Dict
import FamilyDict

main =
    Html.text "Hello, World"


type Family
    = Daan
    | Marlies
    | Sophie
    | Robin
    | Hannah


family_hash : Family -> Int
family_hash member =
    case member of
        Daan ->
            0

        Marlies ->
            1

        Sophie ->
            2

        Robin ->
            3

        Hannah ->
            4


type Hat
    = Red
    | Orange
    | Yellow
    | Green
    | Blue



type alias Logigram =
    { current : FamilyDict.FamilyDict Family Hat
    , target : FamilyDict.FamilyDict Family Hat
    }


init : Logigram
init =
    let
        target =
            FamilyDict.empty family_hash
                |> FamilyDict.insert Daan Red
                |> FamilyDict.insert Marlies Orange
                |> FamilyDict.insert Sophie Yellow
                |> FamilyDict.insert Robin Green
                |> FamilyDict.insert Hannah Blue
    in
        { current = FamilyDict.empty family_hash, target = target }
