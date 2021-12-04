-- https://adventofcode.com/2021/day/3


module Day.Three exposing (partOne)

import List.Extra as List
import Maybe.Extra as Maybe



-- PART ONE


partOne : List String -> String
partOne inputs =
    let
        binGammaRate =
            getGammaRate inputs

        binEpsilonRate =
            binGammaRate |> invertBinary

        powerConsumption =
            binaryToInt binGammaRate * binaryToInt binEpsilonRate
    in
    String.fromInt powerConsumption


getGammaRate : List String -> List Int
getGammaRate binaries =
    let
        bitCount =
            List.head binaries
                |> Maybe.unwrap 0 String.length
    in
    binaries
        |> List.map (String.split "" >> List.map String.toInt >> Maybe.values)
        |> List.foldl
            (\vals acc ->
                List.map2
                    (\val accVal ->
                        case val of
                            1 ->
                                accVal + 1

                            0 ->
                                accVal - 1

                            _ ->
                                accVal
                    )
                    vals
                    acc
            )
            (List.initialize bitCount (\_ -> 0))
        |> List.map (Basics.clamp 0 1)


invertBinary : List Int -> List Int
invertBinary binary =
    binary
        |> List.map
            (\bit ->
                case bit of
                    1 ->
                        0

                    0 ->
                        1

                    _ ->
                        bit
            )


binaryToInt : List Int -> Int
binaryToInt binary =
    let
        bitCount =
            List.length binary
    in
    binary
        |> List.indexedMap (\index val -> ( bitCount - index - 1, val ))
        |> List.foldl
            (\( power, val ) acc ->
                case val of
                    1 ->
                        ((val * 2) ^ power) + acc

                    _ ->
                        acc
            )
            0
