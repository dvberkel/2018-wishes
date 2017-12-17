module Combination exposing (..)

import Html
import Html.Attributes as Attribute


main =
    Html.program
        { init = init 37
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd msg )
init target =
    ( { current = 0, target = target }, Cmd.none )


type alias Model =
    { current : Int
    , target : Int
    }


type Message
    = Increase Int
    | Decrease Int


update : Message -> Model -> ( Model, Cmd msg )
update message model =
    ( model, Cmd.none )


view : Model -> Html.Html msg
view model =
    let
        ds =
            (digits 3 model.current)
                |> List.map toString
                |> List.map Html.text
    in
        Html.div [ Attribute.class "combination-lock" ]
            ds


digits : Int -> Int -> List Int
digits levels n =
    List.reverse (reversed_digits levels n)


reversed_digits : Int -> Int -> List Int
reversed_digits levels n =
    if levels == 0 then
        []
    else
        let
            suffix =
                digits (levels - 1) (n // 10)

            digit =
                n % 10
        in
            digit :: suffix


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
