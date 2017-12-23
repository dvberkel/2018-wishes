module Logigram exposing (..)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Navigation exposing (load)
import CustomDict


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
    = Rood
    | Oranje
    | Geel
    | Groen
    | Blauw


hats : List Hat
hats =
    [ Rood, Oranje, Geel, Groen, Blauw ]


type alias Logigram =
    { current : CustomDict.CustomDict Family Hat
    , target : CustomDict.CustomDict Family Hat
    , message : Maybe String
    }


init : Logigram
init =
    let
        target =
            CustomDict.empty family_hash
                |> CustomDict.insert Daan Geel
                |> CustomDict.insert Marlies Blauw
                |> CustomDict.insert Sophie Oranje
                |> CustomDict.insert Robin Rood
                |> CustomDict.insert Hannah Groen
    in
        { current = CustomDict.empty family_hash, target = target, message = Nothing }


type Message
    = Wears Family Hat
    | Check


update : Message -> Logigram -> ( Logigram, Cmd msg )
update message logigram =
    case message of
        Wears member color ->
            let
                next_current =
                    CustomDict.insert member color logigram.current

                next_model =
                    { logigram | current = next_current }
            in
                ( next_model, Cmd.none )

        Check ->
            let
                is_solved =
                    solved logigram

                next_message =
                    if is_solved then
                        Just "Correct"
                    else
                        Just "Incorrect"

                next_model =
                    { logigram | message = next_message }

                next_command =
                    if is_solved then
                        load "celebrate.html"
                    else
                        Cmd.none
            in
                ( next_model, next_command )


solved : Logigram -> Bool
solved logigram =
    let
        correct_hats : Family -> Bool
        correct_hats member =
            CustomDict.get member logigram.current == CustomDict.get member logigram.target
    in
        List.all correct_hats members


view : Logigram -> Html.Html Message
view logigram =
    let
        button =
            check logigram

        entries =
            named_cartesian members hats

        ds =
            button
                :: header
                :: (List.map (view_member logigram.current) entries)
    in
        Html.div [ Attribute.class "logigram" ] ds


check : Logigram -> Html.Html Message
check logigram =
    let
        message =
            Maybe.withDefault "" logigram.message
    in
        Html.div [ Attribute.class "check" ]
            [ Html.button [ Event.onClick Check ]
                [ Html.text "enter" ]
            , Html.span
                [ Attribute.class "message" ]
                [ Html.text message ]
            ]


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


header : Html.Html msg
header =
    let
        dummy =
            Html.span [ Attribute.class "dummy" ] []

        hs =
            List.map view_hat_name hats

        ds =
            dummy :: hs
    in
        Html.div [ Attribute.class "header" ] ds


view_member : CustomDict.CustomDict Family Hat -> ( Family, List ( Family, Hat ) ) -> Html.Html Message
view_member dictionary ( member, hats ) =
    let
        name =
            Html.span [ Attribute.class "name" ] [ Html.text (toString member) ]

        ds =
            name :: (List.map (view_hat dictionary) hats)
    in
        Html.div [ Attribute.class "hats" ] ds


view_hat : CustomDict.CustomDict Family Hat -> ( Family, Hat ) -> Html.Html Message
view_hat dictionary ( member, hat ) =
    let
        chosen =
            case CustomDict.get member dictionary of
                Just chosen_hat ->
                    chosen_hat == hat

                Nothing ->
                    False
    in
        Html.span
            [ Attribute.classList
                [ ( "chosen", chosen )
                , ( "hat", True )
                ]
            , Event.onClick (Wears member hat)
            ]
            []


view_hat_name : Hat -> Html.Html msg
view_hat_name hat =
    Html.span [ Attribute.class "hat-name" ] [ Html.text (toString hat) ]


subscriptions : Logigram -> Sub msg
subscriptions _ =
    Sub.none
