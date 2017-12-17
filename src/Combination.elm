module Combination exposing (..)

import Html


main =
    Html.program {
            init = init 37
                , update = update
                , view= view
                , subscriptions= subscriptions
        }


init: Int -> (Model, Cmd msg)
init target =
    ({ current= 0, target= target}, Cmd.none)

type alias Model =
    { current : Int
    , target : Int
    }


type Message
    = Increase Int
    | Decrease Int


update : Message -> Model -> (Model, Cmd msg)
update message model =
    (model, Cmd.none)


view : Model -> Html.Html msg
view model =
    Html.text "Hello, World!"


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
