module Logigram exposing (..)

import Html
import Html.Attributes as Attribute
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
    = Rood
    | Oranje
    | Geel
    | Groen
    | Blauw


hats : List Hat
hats =
    [ Rood, Oranje, Geel, Groen, Blauw ]


type alias Logigram =
    { current : FamilyDict.FamilyDict Family Hat
    , target : FamilyDict.FamilyDict Family Hat
    , message : Maybe String
    }


init : Logigram
init =
    let
        target =
            FamilyDict.empty family_hash
                |> FamilyDict.insert Daan Rood
                |> FamilyDict.insert Marlies Oranje
                |> FamilyDict.insert Sophie Geel
                |> FamilyDict.insert Robin Groen
                |> FamilyDict.insert Hannah Blauw
    in
        { current = FamilyDict.empty family_hash, target = target, message = Nothing }


type Message
    = Wears Family Hat
    | Check


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

        Check ->
            let
                next_message =
                    if solved logigram then
                        Just "Correct"
                    else
                        Just "Incorrect"

                next_model =
                    { logigram | message = next_message }
            in
                ( next_model, Cmd.none )


solved : Logigram -> Bool
solved logigram =
    let
        correct_hats : Family -> Bool
        correct_hats member =
            FamilyDict.get member logigram.current == FamilyDict.get member logigram.target
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
            button ::
            header ::
            (List.map (view_member logigram.current) entries)
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


view_member : FamilyDict.FamilyDict Family Hat -> ( Family, List ( Family, Hat ) ) -> Html.Html Message
view_member dictionary ( member, hats ) =
    let
        name =
            Html.span [ Attribute.class "name" ] [ Html.text (toString member) ]

        ds =
            name :: (List.map (view_hat dictionary) hats)
    in
        Html.div [ Attribute.class "hats" ] ds


view_hat : FamilyDict.FamilyDict Family Hat -> ( Family, Hat ) -> Html.Html Message
view_hat dictionary ( member, hat ) =
    let
        chosen =
            case FamilyDict.get member dictionary of
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
