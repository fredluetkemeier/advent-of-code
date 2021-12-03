module Day.Two exposing (partOne, partTwo)

import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, int, keyword, spaces)


type Direction
    = Forward Int
    | Up Int
    | Down Int


type alias Position =
    ( Int, Int )



-- PART ONE


partOne : List String -> String
partOne inputs =
    let
        ( x, y ) =
            inputs
                |> List.map parseDirection
                |> Maybe.values
                |> getPosition
    in
    String.fromInt (x * y)


getPosition : List Direction -> Position
getPosition directions =
    case directions of
        (Forward value) :: tail ->
            Tuple.mapFirst (\acc -> acc + value) (getPosition tail)

        (Down value) :: tail ->
            Tuple.mapSecond (\acc -> acc + value) (getPosition tail)

        (Up value) :: tail ->
            Tuple.mapSecond (\acc -> acc - value) (getPosition tail)

        [] ->
            ( 0, 0 )



-- PART TWO


partTwo : List String -> String
partTwo inputs =
    let
        ( x, y ) =
            inputs
                |> List.map parseDirection
                |> Maybe.values
                |> getPositionByAim
    in
    String.fromInt (x * y)


parseDirection : String -> Maybe Direction
parseDirection =
    Parser.run directionParser
        >> Result.toMaybe


directionParser : Parser Direction
directionParser =
    Parser.oneOf
        [ Parser.succeed Forward
            |. keyword "forward"
            |. spaces
            |= int
        , Parser.succeed Up
            |. keyword "up"
            |. spaces
            |= int
        , Parser.succeed Down
            |. keyword "down"
            |. spaces
            |= int
        ]


getPositionByAim : List Direction -> Position
getPositionByAim directions =
    case directions of
        (Forward value) :: tail ->
            Tuple.mapFirst (\acc -> acc + value) (getPosition tail)

        (Down value) :: tail ->
            Tuple.mapSecond (\acc -> acc + value) (getPosition tail)

        (Up value) :: tail ->
            Tuple.mapSecond (\acc -> acc - value) (getPosition tail)

        [] ->
            ( 0, 0 )
