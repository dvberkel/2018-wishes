module Logigram exposing (..)

import Html
import Html.Events as Event
import FamilyDict


main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


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


members : List Family
members =
    [ Daan, Marlies, Sophie, Robin, Hannah ]


type Hat
    = Red
    | Orange
    | Yellow
    | Green
    | Blue


hats : List Hat
hats =
    [ Red, Orange, Yellow, Green, Blue ]


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


type Message
    = Wears Family Hat


update : Message -> Logigram -> ( Logigram, Cmd msg )
update message logigram =
    case message of
        Wears member color ->
            let
                next_current =
                    FamilyDict.insert member color logigram.current

                next_model =
                    { logigram | current = next_current }
            in
                ( next_model, Cmd.none )


view : Logigram -> Html.Html Message
view logigram =
    let
        entries =
            named_cartesian members hats

        ds =
            List.map (view_member logigram.current) entries
    in
        Html.div [] ds


named_cartesian : List a -> List b -> List ( a, List ( a, b ) )
named_cartesian xs ys =
    let
        pair : a -> List ( a, b )
        pair x =
            List.map (x |> (,)) ys

        combine : (a -> b) -> a -> ( a, b )
        combine f x =
            ( x, f x )

        mapper x =
            ( x, pair x )
    in
        List.map mapper xs


view_member : FamilyDict.FamilyDict Family Hat -> ( Family, List ( Family, Hat ) ) -> Html.Html Message
view_member dictionary ( member, hats ) =
    let
        name =
            Html.span [] [ Html.text (toString member) ]

        ds =
            name :: (List.map (view_hat dictionary) hats)
    in
        Html.div [] ds

view_hat : FamilyDict.FamilyDict Family Hat -> ( Family, Hat ) -> Html.Html Message
view_hat dictionary ( member, hat ) =
    let
        text =
            case FamilyDict.get member dictionary of
                Just chosen_hat ->
                    if chosen_hat == hat then
                        "check"
                    else
                        "."
                Nothing -> "."
    in
        Html.span [Event.onClick (Wears member hat)] [ Html.text text ]


subscriptions : Logigram -> Sub msg
subscriptions _ =
    Sub.none
