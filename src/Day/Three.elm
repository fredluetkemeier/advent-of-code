-- https://adventofcode.com/2021/day/3


module Day.Three exposing (partOne, partTwo)

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
        |> List.map binaryFromString
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



-- PART TWO


partTwo : List String -> String
partTwo inputs =
    let
        binaries =
            inputs |> List.map binaryFromString

        testInput =
            [ "00100"
            , "11110"
            , "10110"
            , "10111"
            , "10101"
            , "01111"
            , "00111"
            , "11100"
            , "10000"
            , "11001"
            , "00010"
            , "01010"
            ]
                |> List.map binaryFromString

        first =
            getBitFrequency 0 testInput |> Debug.log "testMostCommon"

        testOxyRating =
            getOxygenRating 0 testInput |> Debug.log "testOxyRating"

        -- testC02Rating =
        --     getC02Rating 0 testInput |> Debug.log "testC02Rating"
        -- oxygenRating =
        --     getOxygenRating 0 binaries |> Debug.log "oxygenRating"
    in
    ""


getOxygenRating : Int -> List (List Int) -> List Int
getOxygenRating pos binaries =
    case binaries of
        head :: [] ->
            head

        _ :: _ ->
            let
                bitCriteria =
                    case binaries |> getBitFrequency pos of
                        Frequency { mostCommon } ->
                            mostCommon

                        Equal ->
                            1
            in
            binaries
                |> List.filter
                    (\bin ->
                        case List.getAt pos bin of
                            Just bit ->
                                bit == bitCriteria

                            Nothing ->
                                False
                    )
                |> getOxygenRating (pos + 1)

        [] ->
            []


type Frequency a
    = Frequency
        { mostCommon : a
        , leastCommon : a
        }
    | Equal


getBitFrequency : Int -> List (List Int) -> Frequency Int
getBitFrequency pos binaries =
    let
        ( zeroCount, oneCount ) =
            binaries
                |> List.map (List.getAt pos)
                |> Maybe.values
                |> List.partition ((==) 0)
                |> Tuple.mapBoth List.length List.length
    in
    case Basics.compare zeroCount oneCount of
        LT ->
            Frequency { mostCommon = 1, leastCommon = 0 }

        GT ->
            Frequency { mostCommon = 0, leastCommon = 1 }

        EQ ->
            Equal


binaryFromString : String -> List Int
binaryFromString =
    String.split ""
        >> List.map String.toInt
        >> Maybe.values
