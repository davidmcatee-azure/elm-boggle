module Main exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Array exposing (..)
import Random exposing (..)
import Random.List exposing (..)
import Random.Extra exposing (..)
import Json.Decode as Json
import Debug exposing (..)
import Maybe.Extra exposing (isJust)
import Regex exposing (..)
import Task


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { boggleVersion : BoggleVersion
    , board : Maybe Board
    , words : List String
    , wordsNotFound : List String
    , inputWord : String
    }


type BoggleVersion
    = Classic
    | New
    | Deluxe


type alias Board =
    Array Row


type alias Row =
    Array String


type alias Position =
    ( Int, Int )


type alias Die =
    List String


type alias Dice =
    List Die


initModel : Model
initModel =
    { boggleVersion = Deluxe
    , board = Nothing
    , words = []
    , wordsNotFound = []
    , inputWord = ""
    }


{-| sources for letter distribution:
    Classic, New:
    http://www.bananagrammer.com/2013/10/the-boggle-cube-redesign-and-its-effect.html
    Deluxe:
    https://boardgamegeek.com/thread/300883/letter-distribution
-}
diceFromVersion : BoggleVersion -> Dice
diceFromVersion boggleVersion =
    case boggleVersion of
        Classic ->
            [ [ "A", "A", "C", "I", "O", "T" ]
            , [ "A", "B", "I", "L", "T", "Y" ]
            , [ "A", "B", "J", "M", "O", "Qu" ]
            , [ "A", "C", "D", "E", "M", "P" ]
            , [ "A", "C", "E", "L", "R", "S" ]
            , [ "A", "D", "E", "N", "V", "Z" ]
            , [ "A", "H", "M", "O", "R", "S" ]
            , [ "B", "I", "F", "O", "R", "X" ]
            , [ "D", "E", "N", "O", "S", "W" ]
            , [ "D", "K", "N", "O", "T", "U" ]
            , [ "E", "E", "F", "H", "I", "Y" ]
            , [ "E", "G", "K", "L", "U", "Y" ]
            , [ "E", "G", "I", "N", "T", "V" ]
            , [ "E", "H", "I", "N", "P", "S" ]
            , [ "E", "L", "P", "S", "T", "U" ]
            , [ "G", "I", "L", "R", "U", "W" ]
            ]

        New ->
            [ [ "A", "A", "E", "E", "G", "N" ]
            , [ "A", "B", "B", "J", "O", "O" ]
            , [ "A", "C", "H", "O", "P", "S" ]
            , [ "A", "F", "F", "K", "P", "S" ]
            , [ "A", "O", "O", "T", "T", "W" ]
            , [ "C", "I", "M", "O", "T", "U" ]
            , [ "D", "E", "I", "L", "R", "X" ]
            , [ "D", "E", "L", "R", "V", "Y" ]
            , [ "D", "I", "S", "T", "T", "Y" ]
            , [ "E", "E", "G", "H", "N", "W" ]
            , [ "E", "E", "I", "N", "S", "U" ]
            , [ "E", "H", "R", "T", "V", "W" ]
            , [ "E", "I", "O", "S", "S", "T" ]
            , [ "E", "L", "R", "T", "T", "Y" ]
            , [ "H", "I", "M", "N", "U", "Qu" ]
            , [ "H", "L", "N", "N", "R", "Z" ]
            ]

        Deluxe ->
            [ [ "A", "A", "A", "F", "R", "S" ]
            , [ "A", "A", "E", "E", "E", "E" ]
            , [ "A", "A", "F", "I", "R", "S" ]
            , [ "A", "D", "E", "N", "N", "N" ]
            , [ "A", "E", "E", "E", "E", "M" ]
            , [ "A", "E", "E", "G", "M", "U" ]
            , [ "A", "E", "G", "M", "N", "N" ]
            , [ "A", "F", "I", "R", "S", "Y" ]
            , [ "B", "J", "K", "Qu", "X", "Z" ]
            , [ "C", "C", "N", "S", "T", "W" ]
            , [ "C", "E", "I", "I", "L", "T" ]
            , [ "C", "E", "I", "L", "P", "T" ]
            , [ "C", "E", "I", "P", "S", "T" ]
            , [ "D", "H", "H", "N", "O", "T" ]
            , [ "D", "H", "H", "L", "O", "R" ]
            , [ "D", "H", "L", "N", "O", "R" ]
            , [ "D", "D", "L", "N", "O", "R" ]
            , [ "E", "I", "I", "I", "T", "T" ]
            , [ "E", "M", "O", "T", "T", "T" ]
            , [ "E", "N", "S", "S", "S", "U" ]
            , [ "F", "I", "P", "R", "S", "Y" ]
            , [ "G", "O", "R", "R", "V", "W" ]
            , [ "H", "I", "P", "R", "R", "Y" ]
            , [ "N", "O", "O", "T", "U", "W" ]
            , [ "O", "O", "O", "T", "T", "U" ]
            ]


emptyBoard : BoggleVersion -> Board
emptyBoard boggleVersion =
    let
        boardLength =
            diceFromVersion boggleVersion |> sqrtListLength
    in
        Array.repeat boardLength "" |> Array.repeat boardLength



-- UPDATE


type Msg
    = NoOp
    | GenerateBoard
    | BoardGenerated Board
    | InputWord String
    | AddInputWord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GenerateBoard ->
            model
                ! [ randomBoard model.boggleVersion |> generate BoardGenerated ]

        BoardGenerated board ->
            { model
                | board = Just board
                , words = []
                , wordsNotFound = []
                , inputWord = ""
            }
                ! [ Dom.focus "inputWord" |> taskIgnoreResult ]

        InputWord inputWord ->
            { model | inputWord = (stripSpaces << String.toLower) inputWord }
                ! []

        AddInputWord ->
            addInputWord model ! []


randomBoard : BoggleVersion -> Generator Board
randomBoard boggleVersion =
    let
        dice =
            diceFromVersion boggleVersion

        boardLength =
            sqrtListLength dice
    in
        (combine << sampleDice) dice
            |> andThen shuffle
            |> Random.map (fromList << splitListToArrays boardLength)


sqrtListLength : List a -> Int
sqrtListLength =
    round << sqrt << toFloat << List.length


sampleDice : Dice -> List (Generator String)
sampleDice =
    List.map sampleDie


sampleDie : Die -> Generator String
sampleDie die =
    sample die |> Random.map (Maybe.withDefault "")


splitListToArrays : Int -> List a -> List (Array a)
splitListToArrays n listRemainder =
    if List.length listRemainder > n then
        fromList (List.take n listRemainder) :: (List.drop n listRemainder |> splitListToArrays n)
    else
        [ fromList listRemainder ]


addInputWord : Model -> Model
addInputWord model =
    if String.length model.inputWord < 3 then
        { model | inputWord = "" }
    else
        case model.board of
            Just board ->
                let
                    ( updatedWords, updatedWordsNotFound ) =
                        if List.member model.inputWord model.words || List.member model.inputWord model.wordsNotFound then
                            ( model.words, model.wordsNotFound )
                        else if isWordOnBoard model.inputWord board then
                            ( model.inputWord :: model.words, model.wordsNotFound )
                        else
                            ( model.words, model.inputWord :: model.wordsNotFound )
                in
                    { model
                        | inputWord = ""
                        , words = updatedWords
                        , wordsNotFound = updatedWordsNotFound
                    }

            Nothing ->
                { model | inputWord = "" }


isWordOnBoard : String -> Board -> Bool
isWordOnBoard word board =
    recursiveWordCheck word board []


recursiveWordCheck : String -> Board -> List Position -> Bool
recursiveWordCheck word board positions =
    let
        positionsWord =
            List.map (atPositionIn board) positions
                |> List.map (Maybe.withDefault "")
                |> List.reverse
                |> String.concat

        partialMatch =
            String.startsWith (String.toLower positionsWord) (String.toLower word)

        exactMatch =
            partialMatch && String.length positionsWord == String.length word
    in
        if exactMatch then
            True
        else if partialMatch then
            adjacentNewPositions positions board
                |> List.map (flip (::) positions)
                |> List.any (recursiveWordCheck word board)
        else
            False


atPositionIn : Board -> Position -> Maybe String
atPositionIn board position =
    let
        ( x, y ) =
            position
    in
        Array.get x board
            |> Maybe.andThen (Array.get y)


adjacentNewPositions : List Position -> Board -> List Position
adjacentNewPositions positions board =
    case positions of
        [] ->
            allBoardPositions board

        currentPosition :: lastPositions ->
            adjacentPositions currentPosition board
                |> List.filter (\p -> not (List.member p lastPositions))


allBoardPositions : Board -> List Position
allBoardPositions board =
    Array.indexedMap allRowPositions board
        |> Array.toList
        |> List.concat


allRowPositions : Int -> Row -> List Position
allRowPositions rowIndex row =
    Array.indexedMap (\colIndex _ -> ( rowIndex, colIndex )) row
        |> Array.toList


adjacentPositions : Position -> Board -> List Position
adjacentPositions position board =
    let
        ( x, y ) =
            position

        possiblePositions =
            [ ( x - 1, y - 1 )
            , ( x - 1, y )
            , ( x - 1, y + 1 )
            , ( x, y - 1 )
            , ( x, y + 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y )
            , ( x + 1, y + 1 )
            ]
    in
        filterValidPositions board possiblePositions


filterValidPositions : Board -> List Position -> List Position
filterValidPositions board =
    List.filter (isJust << atPositionIn board)


taskIgnoreResult : Task.Task x a -> Cmd Msg
taskIgnoreResult =
    Task.attempt (always NoOp)


stripSpaces : String -> String
stripSpaces =
    replace All (regex "\\s") (\_ -> "")



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "Game" ]
        [ div []
            [ aboveBoardView
            , lazy2 boardView model.boggleVersion model.board
            , lazy belowBoardView model.inputWord
            ]
        , div []
            [ div [ class "Words" ]
                (wordListsView model)
            ]
        ]


aboveBoardView : Html Msg
aboveBoardView =
    div [ class "AboveBoard" ]
        [ button [ type_ "button", class "ShuffleBtn", onClick GenerateBoard ] [ text "Shuffle" ] ]


boardView : BoggleVersion -> Maybe Board -> Html Msg
boardView version maybeBoard =
    let
        ( className, board ) =
            case maybeBoard of
                Nothing ->
                    ( "Board Board--empty", emptyBoard version )

                Just board ->
                    ( "Board", board )
    in
        div [ class className ]
            (Array.map rowView board |> Array.toList)


rowView : Row -> Html Msg
rowView row =
    div [ class "Board-row" ]
        (Array.map tileView row |> Array.toList)


tileView : String -> Html Msg
tileView str =
    let
        className =
            if String.length str > 1 then
                "Board-tile Board-tile--multi"
            else
                "Board-tile"
    in
        span
            [ class className ]
            [ text str ]


belowBoardView : String -> Html Msg
belowBoardView inputWord =
    div [ class "BelowBoard" ]
        [ input [ class "InputWord", id "inputWord", type_ "text", value inputWord, onInput InputWord, onEnter AddInputWord ] []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        Html.Events.on "keydown" (keyCode |> Json.andThen isEnter)


wordListsView : Model -> List (Html Msg)
wordListsView model =
    let
        words =
            [ List.length model.words
                |> pluralize "{} word" "{} words"
                |> wordsHeader
            , div [ class "Words-list" ]
                [ ul []
                    (List.map wordListItem model.words)
                ]
            ]

        invalidWords =
            if List.isEmpty model.wordsNotFound then
                []
            else
                [ List.length model.wordsNotFound
                    |> pluralize "{} word not found" "{} words not found"
                    |> wordsHeader
                , div [ class "Words-list Words-list--notFound" ]
                    [ ul []
                        (List.map wordListItem model.wordsNotFound)
                    ]
                ]
    in
        List.append words invalidWords


pluralize : String -> String -> Int -> String
pluralize one other count =
    let
        insertCount =
            replace All (regex "{}") (\_ -> toString count)
    in
        case count of
            1 ->
                insertCount one

            _ ->
                insertCount other


wordsHeader : String -> Html Msg
wordsHeader title =
    div [ class "Words-header" ] [ text title ]


wordListItem : String -> Html Msg
wordListItem word =
    li [ class "Words-listItem" ] [ text word ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
