module Combination exposing (..)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Navigation exposing (load)

main =
    Html.program
        { init = init 80
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd msg )
init target =
    ( { current = 0, max_digits = 3, target = target, message = Nothing }, Cmd.none )


type alias Model =
    { current : Int
    , max_digits : Int
    , target : Int
    , message : Maybe String
    }


type Message
    = Increase Int
    | Decrease Int
    | Check


update : Message -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Check ->
            if model.current == model.target then
                ( { model | message = Just "correct" }, load "two.html" )
            else
                ( { model | message = Just "incorrect" }, Cmd.none )

        Increase position ->
            ( { model | message = Nothing, current = (model.current + 10 ^ position) % 10 ^ model.max_digits }
            , Cmd.none
            )

        Decrease position ->
            ( { model | message = Nothing, current = (model.current - 10 ^ position) % 10 ^ model.max_digits }
            , Cmd.none
            )


view : Model -> Html.Html Message
view model =
    let
        message =
            Maybe.withDefault "" model.message

        ds =
            model.current
                |> reversed_digits model.max_digits
                |> List.indexedMap view_digit
                |> List.reverse
    in
        Html.div [ Attribute.class "combination-lock" ]
            [ Html.div [ Attribute.class "message" ] [ Html.text message ]
            , Html.div [ Attribute.class "digits" ] ds
            , Html.button [ Event.onClick Check ] [ Html.text "enter" ]
            ]


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
