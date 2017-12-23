module RobotPuzzle exposing (..)

import Html
import Html.Attributes as Attribute
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
    ( { state = load ( 1, 1 ) program
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
    in
        Html.span
            [ Attribute.classList
                [ ( "position", True )
                , ( toString occupation, True )
                , ( "robot-" ++ (toString robot.heading), position == robot_position )
                ]
            ]
            []


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
