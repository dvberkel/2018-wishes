module RobotPuzzle exposing (..)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Time exposing (Time, millisecond, every)
import Navigation
import Robot exposing (..)
import Parser exposing (compile)


main =
    Html.program
        { init = init "LL[5M]" ( 1, 1 ) North
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : String -> Position -> Heading -> ( Model, Cmd Message )
init source origin heading =
    let
        robot =
            create origin heading

        program =
            case compile source of
                Ok p ->
                    Just p

                Err _ ->
                    Nothing

        state =
            Maybe.map (load robot) program
    in
        ( { run = False
          , source = source
          , program = program
          , initial = robot
          , state = state
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
    , source : String
    , program : Maybe Robot.Program
    , initial : Robot
    , state : Maybe ( Robot, ProgramStack )
    , world : World
    }


type Message
    = Step
    | Idle
    | Toggle
    | UpdateSource String


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Idle ->
            ( model, Cmd.none )

        Toggle ->
            let
                next_run =
                    not model.run

                next_state =
                    if not next_run then
                        Maybe.map (load model.initial) model.program
                    else
                        model.state
            in
                ( { model | run = not model.run, state = next_state }, Cmd.none )

        UpdateSource source ->
            let
                program =
                    case compile source of
                        Ok p ->
                            Just p

                        Err _ ->
                            Nothing

                state =
                    Maybe.map (load model.initial) model.program
            in
                ( { model | run = False, source = source, program = program, state = state }, Cmd.none )

        Step ->
            case model.state of
                Nothing ->
                    ( model, Cmd.none )

                Just state ->
                    let
                        next_state =
                            step model.world state

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
                        ( { model | state = Just next_state }, next_command )


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
            , Html.input [ Attribute.defaultValue model.source, Event.onInput UpdateSource ] []
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
            case model.state of
                Just state ->
                    Tuple.first state

                Nothing ->
                    model.initial

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
