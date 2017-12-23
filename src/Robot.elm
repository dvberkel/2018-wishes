module Robot exposing (..)

import CustomDict


type alias World =
    { occupations : CustomDict.CustomDict Position Occupation
    , bounding_box : Position
    }


emptyWorld : World
emptyWorld =
    { occupations = CustomDict.empty position_hash
    , bounding_box = ( 0, 0 )
    }


position_hash : Position -> Int
position_hash ( x, y ) =
    1997 * x + y


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
    CustomDict.get position world.occupations
        |> Maybe.withDefault Free


parse : String -> World
parse input =
    let
        lines =
            String.lines input

        characters =
            List.map String.toList lines

        occupation_lines =
            List.indexedMap toOccupationLine characters

        occupations =
            List.concat occupation_lines
    in
        fill occupations


toOccupationLine : Int -> List Char -> List ( Int, Int, Occupation )
toOccupationLine y line =
    List.indexedMap (toOccupation y) line


toOccupation : Int -> Int -> Char -> ( Int, Int, Occupation )
toOccupation y x char =
    let
        occupation =
            case char of
                '#' ->
                    Wall

                _ ->
                    Free
    in
        ( x, y, occupation )


fill : List ( Int, Int, Occupation ) -> World
fill occupations =
    List.foldl filler emptyWorld occupations


filler : ( Int, Int, Occupation ) -> World -> World
filler ( x, y, occupation ) world =
    let
        position =
            ( x, y )

        occupations =
            CustomDict.insert position occupation world.occupations

        bounding_box =
            max_bounding_box position world.bounding_box
    in
        { occupations = occupations, bounding_box = bounding_box }


max_bounding_box : Position -> Position -> Position
max_bounding_box ( x, y ) ( u, v ) =
    ( max x u, max y v )


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


load : Position -> Program -> ( Robot, ProgramStack )
load position program =
    ( { heading = North, position = position }, { program = List.singleton program } )


act : World -> Action -> Robot -> Robot
act world action robot =
    case action of
        Idle ->
            robot

        Move ->
            let
                change =
                    delta robot.heading

                candidate =
                    add robot.position change

                heading =
                    turnLeft robot.heading
            in
                case getOccupation world candidate of
                    Free ->
                        { robot | position = add robot.position change }

                    Wall ->
                        { robot | heading = heading }

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
            ( 0, -1 )

        East ->
            ( 1, 0 )

        South ->
            ( 0, 1 )

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


step : World -> ( Robot, ProgramStack ) -> ( Robot, ProgramStack )
step world ( robot, stack ) =
    let
        ( candidate, next_stack ) =
            pop stack

        next_robot =
            case candidate of
                Just action ->
                    act world action robot

                Nothing ->
                    robot
    in
        ( next_robot, next_stack )
