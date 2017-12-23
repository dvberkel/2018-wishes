module RobotPuzzle exposing (..)

import Html
import Html.Events as Event
import Robot exposing (..)


main =
    Html.program
        { init = init (Repeat 4 (Primitive Move))
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Robot.Program -> ( Model, Cmd Message )
init program =
    ( { state = load (1,1) program
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
        [ Html.button [ Event.onClick Step ] [ Html.text ">" ]
        , Html.span [] [ Html.text (toString model.state) ]
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
