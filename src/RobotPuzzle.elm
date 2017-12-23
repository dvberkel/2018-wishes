module RobotPuzzle exposing (..)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Time exposing (Time, millisecond, every)
import Navigation
import Robot exposing (..)


main =
    Html.program
        { init = init (Repeat 100 (Primitive Move))
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Robot.Program -> ( Model, Cmd Message )
init program =
    ( { run = False
      , state = load ( 1, 1 ) program
      , world = """################################################
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                              #
#                                             G#
################################################
""" |> parse
      }
    , Cmd.none
    )


type alias Model =
    { run : Bool
    , state : ( Robot, ProgramStack )
    , world : World
    }


type Message
    = Step
    | Idle
    | Toggle


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Idle ->
            ( model, Cmd.none )

        Toggle ->
            ( { model | run = not model.run }, Cmd.none )

        Step ->
            let
                next_state =
                    step model.world model.state

                robot_position =
                    next_state
                        |> Tuple.first
                        |> .position

                finished =
                    Goal == (getOccupation model.world robot_position)

                next_command =
                    if finished then
                        Navigation.load "celebrate.html"
                    else
                        Cmd.none
            in
                ( { model | state = next_state }, next_command )


view : Model -> Html.Html Message
view model =
    let
        runText =
            if model.run then
                "||"
            else
                ">"
    in
    Html.div []
        [ Html.button [ Event.onClick Toggle ] [ Html.text runText ]
        , Html.span [] [ Html.text (toString model.state) ]
        , viewWorld model
        ]


viewWorld : Model -> Html.Html Message
viewWorld model =
    let
        world =
            model.world

        ( _, max_y ) =
            world.bounding_box

        ys =
            List.range 0 max_y

        ds =
            List.map (viewWorldRow model) ys
    in
        Html.div [ Attribute.class "world" ] ds


viewWorldRow : Model -> Int -> Html.Html Message
viewWorldRow model y =
    let
        world =
            model.world

        ( max_x, _ ) =
            world.bounding_box

        xs =
            List.range 0 max_x

        ds =
            List.map (viewWorldPosition model y) xs
    in
        Html.div [ Attribute.class "row" ] ds


viewWorldPosition : Model -> Int -> Int -> Html.Html Message
viewWorldPosition model y x =
    let
        world =
            model.world

        position =
            ( x, y )

        occupation =
            getOccupation world position

        robot =
            model.state |> Tuple.first

        robot_position =
            robot.position

        representation =
            if position == robot_position then
                case robot.heading of
                    North ->
                        "▲"

                    East ->
                        "▶"

                    South ->
                        "▼"

                    West ->
                        "◀"
            else
                ""
    in
        Html.span
            [ Attribute.classList
                [ ( "position", True )
                , ( toString occupation, True )
                , ( "robot", position == robot_position )
                ]
            ]
            [ Html.text representation ]


subscriptions : Model -> Sub Message
subscriptions model =
    every (100 * millisecond) (takeStep model)

takeStep : Model -> Time -> Message
takeStep model _ =
    if model.run then
        Step
    else
        Idle
