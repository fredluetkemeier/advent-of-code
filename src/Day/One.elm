-- https://adventofcode.com/2021/day/1


module Day.One exposing (partOne, partTwo)

import Maybe.Extra as Maybe



-- PART ONE


partOne : List String -> String
partOne inputs =
    inputs
        |> List.map String.toInt
        |> Maybe.values
        |> countPositiveDeltas
        |> String.fromInt


countPositiveDeltas : List Int -> Int
countPositiveDeltas values =
    case values of
        first :: ((second :: _) as tail) ->
            if second > first then
                1 + countPositiveDeltas tail

            else
                countPositiveDeltas tail

        _ ->
            0



-- PART TWO


partTwo : List String -> String
partTwo inputs =
    inputs
        |> List.map String.toInt
        |> Maybe.values
        |> countPositiveDeltaRanges
        |> String.fromInt


countPositiveDeltaRanges : List Int -> Int
countPositiveDeltaRanges values =
    case values of
        first :: ((_ :: _ :: fourth :: _) as tail) ->
            let
                count =
                    if fourth > first then
                        1

                    else
                        0
            in
            count + countPositiveDeltaRanges tail

        _ ->
            0
