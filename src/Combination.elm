module Combination exposing (..)

import Html
import Html.Attributes as Attribute
import Html.Events as Event


main =
    Html.program
        { init = init 37
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd msg )
init target =
    ( { current = 0, max_digits = 3, target = target }, Cmd.none )


type alias Model =
    { current : Int
    , max_digits : Int
    , target : Int
    }


type Message
    = Increase Int
    | Decrease Int


update : Message -> Model -> ( Model, Cmd msg )
update message model =
    let
        next_candidate =
            case message of
                Increase position ->
                    model.current + 10 ^ position

                Decrease position ->
                    model.current - 10 ^ position

        next_current =
            next_candidate % 10 ^ model.max_digits

        next_model =
            { model | current = next_current }
    in
        ( next_model, Cmd.none )


view : Model -> Html.Html Message
view model =
    let
        ds =
            model.current
                |> reversed_digits model.max_digits
                |> List.indexedMap view_digit
                |> List.reverse
    in
        Html.div [ Attribute.class "combination-lock" ]
            ds


reversed_digits : Int -> Int -> List Int
reversed_digits levels n =
    if levels == 0 then
        []
    else
        let
            suffix =
                reversed_digits (levels - 1) (n // 10)

            digit =
                n % 10
        in
            digit :: suffix


view_digit : Int -> Int -> Html.Html Message
view_digit position digit =
    Html.div [ Attribute.class "digit-control" ]
        [ Html.span [ Attribute.class "up", Event.onClick (Increase position) ] [ Html.text "+" ]
        , Html.span [ Attribute.class "digit" ] [ Html.text (toString digit) ]
        , Html.span [ Attribute.class "down", Event.onClick (Decrease position) ] [ Html.text "-" ]
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
