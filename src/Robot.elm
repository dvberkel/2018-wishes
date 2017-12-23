module Robot exposing (..)

import Dict
import FamilyDict


type alias World =
    FamilyDict.FamilyDict Position Occupation


type Occupation
    = Free
    | Wall


type alias Robot =
    { heading : Heading
    , position : Position
    }


type Heading
    = North
    | East
    | South
    | West


type alias Position =
    ( Int, Int )


type Program
    = Primitive Action
    | Sequence (List Program)
    | Repeat Int Program


type Action
    = Idle
    | Move
    | Left
    | Right


type alias ProgramStack =
    { program : List Program
    }


getOccupation : World -> Position -> Occupation
getOccupation world position =
    FamilyDict.get position world
    |> Maybe.withDefault Free

isEmpty : ProgramStack -> Bool
isEmpty stack =
    List.isEmpty stack.program


pop : ProgramStack -> ( Maybe Action, ProgramStack )
pop stack =
    if isEmpty stack then
        ( Nothing, stack )
    else
        let
            head =
                List.head stack.program

            t =
                List.tail stack.program

            candidate =
                Maybe.withDefault (Primitive Idle) head

            tail =
                Maybe.withDefault [] t
        in
            case candidate of
                Primitive step ->
                    ( Just step, { program = tail } )

                Sequence programs ->
                    let
                        next_stack =
                            { program = List.append programs tail }
                    in
                        pop next_stack

                Repeat n program ->
                    if n > 0 then
                        let
                            header =
                                program :: (Repeat (n - 1) program) :: []

                            next_stack =
                                { program = List.append header tail }
                        in
                            pop next_stack
                    else
                        let
                            next_stack =
                                { program = tail }
                        in
                            pop next_stack


push : Program -> ProgramStack -> ProgramStack
push program stack =
    { program = program :: stack.program }


load : Program -> ( Robot, ProgramStack )
load program =
    ( { heading = North, position = ( 0, 0 ) }, { program = List.singleton program } )


act : Action -> Robot -> Robot
act action robot =
    case action of
        Idle ->
            robot

        Move ->
            let
                change =
                    delta robot.heading
            in
                { robot | position = add robot.position change }

        Left ->
            let
                heading =
                    turnLeft robot.heading
            in
                { robot | heading = heading }

        Right ->
            let
                heading =
                    turnRight robot.heading
            in
                { robot | heading = heading }


delta : Heading -> Position
delta h =
    case h of
        North ->
            ( 0, 1 )

        East ->
            ( 1, 0 )

        South ->
            ( 0, -1 )

        West ->
            ( -1, 0 )


add : Position -> Position -> Position
add ( x, y ) ( dx, dy ) =
    ( x + dx, y + dy )


turnLeft : Heading -> Heading
turnLeft h =
    case h of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


turnRight : Heading -> Heading
turnRight h =
    case h of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


step : ( Robot, ProgramStack ) -> ( Robot, ProgramStack )
step ( robot, stack ) =
    let
        ( candidate, next_stack ) =
            pop stack

        next_robot =
            case candidate of
                Just action ->
                    act action robot

                Nothing ->
                    robot
    in
        ( next_robot, next_stack )
