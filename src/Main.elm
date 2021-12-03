module Main exposing (program)

import Day.One as One
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
            IO.do (runSolution day)
                (\_ -> IO.return ())

        _ ->
            Process.logErr errorMessage


runSolution : String -> IO (List ())
runSolution day =
    case day of
        "1" ->
            IO.do (readFileLines "./inputs/day1-input1.txt")
                (\values ->
                    IO.combine
                        [ values
                            |> One.partOne
                            |> String.append "Part 1: "
                            |> Process.print
                        , values
                            |> One.partTwo
                            |> String.append "Part 2: "
                            |> Process.print
                        ]
                )

        "2" ->
            IO.do (readFileLines "./inputs/day2-input1.txt")
                (\values ->
                    IO.combine
                        [ values
                            |> Two.partOne
                            |> String.append "Part 1: "
                            |> Process.print
                        , values
                            |> Two.partTwo
                            |> String.append "Part 2: "
                            |> Process.print
                        ]
                )

        _ ->
            Process.logErr errorMessage
                |> List.singleton
                |> IO.combine


readFileLines : String -> IO (List String)
readFileLines =
    File.contentsOf
        >> IO.exitOnError identity
        >> IO.map (String.split "\n")


errorMessage : String
errorMessage =
    """
    -----------------------
     ERROR: Invalid args!

     Usage: elm-cli run src/Main.elm <day>
    -----------------------
    """
