module Day.Two exposing (partOne)

import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, end, int, keyword, spaces, token)


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
                |> calculatePosition
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


calculatePosition : List Direction -> Position
calculatePosition directions =
    case directions of
        (Forward value) :: tail ->
            Tuple.mapFirst (\acc -> acc + value) (calculatePosition tail)

        (Down value) :: tail ->
            Tuple.mapSecond (\acc -> acc + value) (calculatePosition tail)

        (Up value) :: tail ->
            Tuple.mapSecond (\acc -> acc - value) (calculatePosition tail)

        [] ->
            ( 0, 0 )



-- PART TWO
