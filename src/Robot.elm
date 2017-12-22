module Robot exposing (..)

import Html


main =
    Html.text "Hello, World!"


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
    = Primitive Step
    | Sequence (List Program)
    | Repeat Int Program


type Step
    = Idle
    | Move
    | Left
    | Right


type alias ProgramStack =
    { program : List Program
    }


isEmpty : ProgramStack -> Bool
isEmpty stack =
    List.isEmpty stack.program


pop : ProgramStack -> ( Maybe Step, ProgramStack )
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
