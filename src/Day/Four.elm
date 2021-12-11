-- https://adventofcode.com/2021/day/4


module Day.Four exposing (partOne)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..))


testInput : String
testInput =
    """7,4,6

22 13 17 11  0
 8  2 23  4 24

 9 18 17 21  0
18  2 23 24 24"""



--     """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19
--  3 15  0  2 22
--  9 18 13 17  5
-- 19  8  7 25 23
-- 20 11 10 24  4
-- 14 21 16 12  6
-- 14 21 17 24  4
-- 10 16 15  9 19
-- 18  8 23 26 20
-- 22 11 13  6  5
--  2  0 12  3  7"""


partOne : String -> String
partOne fileContents =
    let
        parsedInput =
            testInput
                |> String.filter (\c -> c /= '\u{000D}')
                |> Debug.log "input"
                |> Parser.run parseInput
                |> Debug.log "parsedInput"
    in
    ""


type alias Input =
    { drawNumbers : List Int
    , boards : List Board
    }


type alias Board =
    List Row


type alias Row =
    List Int


parseInput : Parser Input
parseInput =
    Parser.succeed Input
        |= parseDrawNumbers
        |. Parser.spaces
        |= parseBoards


parseDrawNumbers : Parser (List Int)
parseDrawNumbers =
    Parser.loop [] <|
        \numbers ->
            Parser.oneOf
                [ Parser.succeed (\num -> Loop (num :: numbers))
                    |= Parser.int
                    |. Parser.oneOf
                        [ Parser.symbol ","
                        , newline
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Done (List.reverse numbers))
                ]


parseBoards : Parser (List Board)
parseBoards =
    Parser.loop [] <|
        \boards ->
            Parser.succeed (\board next -> next (board :: boards))
                |= parseBoard
                |= Parser.oneOf
                    [ Parser.succeed (Done << List.reverse)
                        |. Parser.end
                    , Parser.succeed Loop
                    ]


parseBoard : Parser Board
parseBoard =
    Parser.loop [] <|
        \rows ->
            Parser.succeed (\row next -> next (row :: rows))
                |= parseRow
                |= Parser.oneOf
                    [ Parser.succeed (Done << List.reverse)
                        |. newline
                    , Parser.succeed Loop
                    ]


parseRow : Parser Row
parseRow =
    Parser.loop [] <|
        \numbers ->
            Parser.succeed (\num next -> next (num :: numbers))
                |. Parser.chompWhile (\c -> c == ' ')
                |= Parser.int
                |= Parser.oneOf
                    [ Parser.succeed (Done << List.reverse)
                        |. newline
                    , Parser.succeed Loop
                        |. Parser.symbol " "
                    ]


newline : Parser ()
newline =
    Parser.chompIf isNewline


isNewline : Char -> Bool
isNewline char =
    char == '\n'
