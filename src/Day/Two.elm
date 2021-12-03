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
            Tuple.mapFirst ((+) value) <|
                getPosition tail

        (Down value) :: tail ->
            Tuple.mapSecond ((+) value) <|
                getPosition tail

        (Up value) :: tail ->
            Tuple.mapSecond (\acc -> acc - value) <|
                getPosition tail

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
                |> getPositionByAim 0
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


getPositionByAim : Int -> List Direction -> Position
getPositionByAim aim directions =
    case directions of
        (Forward value) :: tail ->
            Tuple.mapBoth
                ((+) value)
                ((+) (value * aim))
            <|
                getPositionByAim aim tail

        (Down value) :: tail ->
            getPositionByAim (aim + value) tail

        (Up value) :: tail ->
            getPositionByAim (aim - value) tail

        [] ->
            ( 0, 0 )
