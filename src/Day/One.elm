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
        first :: ((second :: third :: fourth :: _) as tail) ->
            let
                sumA =
                    List.sum [ first, second, third ]

                sumB =
                    List.sum [ second, third, fourth ]
            in
            if sumB > sumA then
                1 + countPositiveDeltaRanges tail

            else
                countPositiveDeltaRanges tail

        _ ->
            0
