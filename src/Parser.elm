module Parser exposing (compile)

import Combine exposing (..)
import Combine.Num exposing (int)
import Robot exposing (Program(..), Action(..))


compile : String -> Result String Robot.Program
compile input =
    case parse program input of
        Ok ( _, _, p ) ->
            Ok p

        Err ( _, _, errors ) ->
            Err (String.join " or " errors)


program : Parser s Robot.Program
program =
    lazy
        (\() ->
            choice [ repetition, seq ]
                <* end
        )


primitive : Parser s Robot.Program
primitive =
    choice [ move, left, right ]


move : Parser s Robot.Program
move =
    string "M" $> Primitive Move


left : Parser s Robot.Program
left =
    string "L" $> Primitive Left


right : Parser s Robot.Program
right =
    string "R" $> Primitive Right


seq : Parser s Robot.Program
seq =
    let
        options =
            choice [ primitive, repetition ]

        programs =
            many1 options
                |> map Sequence
    in
        lazy (\() -> programs)


repetition : Parser s Robot.Program
repetition =
    let
        repeatProgram : Parser s Robot.Program
        repeatProgram =
            int |> andThen what

        what : Int -> Parser s Robot.Program
        what n =
            primitive |> map (Repeat n)
    in
        lazy (\() -> brackets repeatProgram)
