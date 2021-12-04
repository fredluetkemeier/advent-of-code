module Main exposing (program)

import Day.One as One
import Day.Three as Three
import Day.Two as Two
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Process


program : Process -> IO ()
program process =
    IO.do (handleArgs process.argv)
        (\_ -> IO.return ())


handleArgs : List String -> IO ()
handleArgs args =
    case args of
        [ _, day ] ->
            IO.do (solutionFromDay day)
                (\_ -> IO.return ())

        _ ->
            Process.logErr errorMessage


solutionFromDay : String -> IO (List ())
solutionFromDay day =
    case day of
        "1" ->
            IO.do (readFileLines "./inputs/day1-input1.txt") <|
                executeParts [ One.partOne, One.partTwo ]

        "2" ->
            IO.do (readFileLines "./inputs/day2-input1.txt") <|
                executeParts [ Two.partOne, Two.partTwo ]

        "3" ->
            IO.do (readFileLines "./inputs/day3-input1.txt") <|
                executeParts [ Three.partOne ]

        _ ->
            Process.logErr errorMessage
                |> List.singleton
                |> IO.combine


readFileLines : String -> IO (List String)
readFileLines =
    File.contentsOf
        >> IO.exitOnError identity
        >> IO.map (String.split "\n")


executeParts : List (List String -> String) -> List String -> IO (List ())
executeParts parts inputs =
    parts
        |> List.indexedMap
            (\index part ->
                inputs
                    |> part
                    |> String.append ("Part " ++ String.fromInt (index + 1) ++ ": ")
                    |> Process.print
            )
        |> IO.combine


errorMessage : String
errorMessage =
    """
    -----------------------
     ERROR: Invalid args!

     Usage: elm-cli run src/Main.elm <day>
    -----------------------
    """
