module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events


type alias Model =
    { gameState : GameState
    }


type alias PlayingModel =
    { word : List Char
    , guessedChars : List Char
    , attemptsLeft : Int
    }


type GameState
    = Playing PlayingModel
    | Failed String
    | Succeeded String


type Msg
    = InputChanged String
    | Reset


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { gameState =
        Playing
            { word = String.toList "hangman"
            , guessedChars = []
            , attemptsLeft = 10
            }
    }


view : Model -> Html Msg
view model =
    case model.gameState of
        Playing playingModel ->
            viewPlaying playingModel

        Failed word ->
            div
                []
                [ text "You failed"
                , button [ Events.onClick Reset ] [ text "Reset" ]
                ]

        Succeeded word ->
            div
                []
                [ text "You won"
                , button [ Events.onClick Reset ] [ text "Reset" ]
                ]


viewPlaying : PlayingModel -> Html Msg
viewPlaying playingModel =
    div
        []
        [ viewHangman playingModel
        , viewInput playingModel
        , viewCounter playingModel.attemptsLeft
        ]


viewCounter : Int -> Html Msg
viewCounter attemptsLeft =
    text (String.fromInt attemptsLeft)


viewHangman : PlayingModel -> Html Msg
viewHangman playingModel =
    ul
        []
        (List.map (viewCharacter playingModel.guessedChars) playingModel.word)


viewInput : PlayingModel -> Html Msg
viewInput playingModel =
    input
        [ Events.onInput InputChanged
        , Attr.value ""
        ]
        []


viewCharacter : List Char -> Char -> Html Msg
viewCharacter guessedChars char =
    let
        asCharacter : String
        asCharacter =
            if List.member char guessedChars then
                String.fromChar char

            else
                "_"
    in
    li
        [ Attr.style "display" "inline"
        , Attr.style "padding" "5px"
        ]
        [ text asCharacter
        ]


update : Msg -> Model -> Model
update msg model =
    case ( model.gameState, msg ) of
        ( _, Reset ) ->
            init

        ( Playing playingModel, InputChanged newInput ) ->
            case List.head (String.toList newInput) of
                Nothing ->
                    model

                Just char ->
                    let
                        newPlayingModel =
                            { playingModel
                                | guessedChars = char :: playingModel.guessedChars
                                , attemptsLeft = playingModel.attemptsLeft - 1
                            }

                        newModel =
                            if newPlayingModel.attemptsLeft <= 0 then
                                { model | gameState = Failed (String.fromList newPlayingModel.word) }

                            else if wordGuessed newPlayingModel then
                                { model | gameState = Succeeded (String.fromList newPlayingModel.word) }

                            else
                                { model | gameState = Playing newPlayingModel }
                    in
                    newModel

        ( _, _ ) ->
            model


wordGuessed : PlayingModel -> Bool
wordGuessed playingModel =
    List.all
        (\char ->
            List.member char playingModel.guessedChars
        )
        playingModel.word
