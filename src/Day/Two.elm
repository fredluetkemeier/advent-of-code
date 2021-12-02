module Day.Two exposing (partOne)

import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, end, int, keyword, spaces, token)


type Direction
    = Forward Int
    | Up Int
    | Down Int


type alias Position =
    ( Int, Int )


partOne : List String -> String
partOne inputs =
    let
        -- debug =
        --     inputs
        --         |> List.map parseDirection
        --         |> Maybe.values
        --         |> Debug.log "debug"
        ( x, y ) =
            inputs
                |> List.map parseDirection
                |> Maybe.values
                |> calculatePosition
    in
    String.fromInt x ++ " " ++ String.fromInt y


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

        _ ->
            ( 0, 0 )
