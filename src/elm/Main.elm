module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (..)
import Random exposing (..)
import Random.List exposing (..)
import Random.Extra exposing (..)


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


type alias Die =
    List String


type alias Dice =
    List Die


{-| source for letter distribution:
    http://www.bananagrammer.com/2013/10/the-boggle-cube-redesign-and-its-effect.html
-}
dice : Dice
dice =
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


type alias Model =
    { board : Maybe Board
    }


type alias Board =
    Array Row


type alias Row =
    Array String



-- type alias Position =
--     ( Int, Int )


emptyBoard : Board
emptyBoard =
    Array.repeat 4 ""
        |> Array.repeat 4


initModel : Model
initModel =
    { board = Nothing
    }



-- UPDATE


splitListToArrays : Int -> List a -> List (Array a)
splitListToArrays n listRemainder =
    if List.length listRemainder > n then
        fromList (List.take n listRemainder) :: (List.drop n listRemainder |> splitListToArrays n)
    else
        [ fromList listRemainder ]


sampleDie : Die -> Generator String
sampleDie die =
    sample die |> Random.map (Maybe.withDefault "")


sampleDice : Dice -> List (Generator String)
sampleDice =
    List.map sampleDie


randomBoard : Generator Board
randomBoard =
    (combine << sampleDice) dice
        |> andThen shuffle
        |> Random.map (fromList << splitListToArrays 4)


type Msg
    = GenerateBoard
    | BoardGenerated Board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBoard ->
            ( model, generate BoardGenerated randomBoard )

        BoardGenerated board ->
            ( { model | board = Just board }, Cmd.none )



-- VIEW


tileView : String -> Html Msg
tileView str =
    span [ class "Board-tile" ] [ text str ]


rowView : Row -> Html Msg
rowView row =
    div [ class "Board-row" ]
        (Array.map tileView row |> Array.toList)


boardView : Maybe Board -> Html Msg
boardView maybeBoard =
    let
        ( className, board ) =
            case maybeBoard of
                Nothing ->
                    ( "Board Board--empty", emptyBoard )

                Just board ->
                    ( "Board", board )
    in
        div [ class className ]
            (Array.map rowView board |> Array.toList)


shuffleView : Html Msg
shuffleView =
    div [ class "Shuffle" ]
        [ button [ type_ "button", class "Shuffle-btn", onClick GenerateBoard ] [ text "Shuffle" ] ]


view : Model -> Html Msg
view model =
    div []
        [ shuffleView
        , boardView model.board
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
