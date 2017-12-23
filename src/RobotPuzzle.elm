module RobotPuzzle exposing (..)

import Html
import Html.Events as Event
import Robot exposing (..)


main =
    Html.program
        { init = init (Repeat 4 (Sequence [ Repeat 2 (Primitive Move), Primitive Left ]))
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Robot.Program -> ( Model, Cmd Message )
init program =
    ( { state = load program
      , world = """################################################
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
################################################
""" |> parse
      }
    , Cmd.none
    )


type alias Model =
    { state : ( Robot, ProgramStack )
    , world : World
    }


type Message
    = Step


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Step ->
            let
                next_state =
                    step model.world model.state
            in
                ( { model | state = next_state }, Cmd.none )


view : Model -> Html.Html Message
view model =
    Html.div []
        [ Html.span [] [ Html.text (toString model.state) ]
        , Html.button [ Event.onClick Step ] [ Html.text ">" ]
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
