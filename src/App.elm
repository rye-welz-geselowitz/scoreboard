module App exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, h3, img, li, table, td, text, th, tr)
import Html.Attributes exposing (class)


--MODEL & TYPES


type alias Model =
    { players : List Player
    , scoreData : ScoreData
    }


type alias Player =
    { id : Int
    , name : String
    , age : Int
    , gender : Gender
    }


type Gender
    = Nonbinary
    | Female
    | Male


type alias ScoreData =
    Dict Int (List Int)



--INIT


init : ( Model, Cmd Msg )
init =
    ( { players =
            [ Player 0 "Elana" 27 Female
            , Player 1 "Sam" 17 Nonbinary
            , Player 2 "Sarah" 24 Female
            , Player 3 "Josh" 18 Male
            , Player 4 "Chris" 31 Nonbinary
            , Player 5 "Charlie" 65 Nonbinary
            , Player 6 "Shaymaa" 17 Female
            , Player 7 "Houreidja" 24 Female
            , Player 8 "Sana" 12 Female
            , Player 9 "Ahmad" 68 Male
            , Player 10 "Prateek" 45 Male
            ]
      , scoreData =
            Dict.fromList
                [ ( 0, [ 55, 65, 62 ] )
                , ( 1, [ 32, 44, 22 ] )
                , ( 2, [ 28 ] )
                , ( 3, [] )
                , ( 4, [ 34, 38, 37, 34 ] )
                , ( 5, [ 88, 87, 85, 88, 89 ] )
                , ( 6, [ 55 ] )
                , ( 7, [ 94, 82 ] )
                ]
      }
    , Cmd.none
    )



--UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )



--VIEW


view : Model -> Html Msg
view model =
    let
        personalHighScore =
            getPersonalHighScore model >> Maybe.withDefault 0

        filterNoScores =
            getScores model >> List.isEmpty >> not

        topFemale =
            model.players
                |> List.filter (\p -> p.gender == Female)
                |> List.filter filterNoScores
                |> List.sortWith (comparePlayers personalHighScore)
                |> List.reverse

        childrenScoringOver50 =
            model.players
                |> List.filter (\p -> p.age < 18 && personalHighScore p > 50)
                |> List.sortWith (comparePlayers personalHighScore)
                |> List.reverse
    in
    div []
        [ leaderBoard "Top 10 Female"
            "High Score"
            personalHighScore
            topFemale
        , leaderBoard "Children Scoring Over 50"
            "High Score"
            personalHighScore
            childrenScoringOver50
        ]


leaderBoard : String -> String -> (Player -> Int) -> List Player -> Html Msg
leaderBoard title dataTitle datumFunc players =
    div [ class "leaderboard" ]
        [ h3 [] [ text title ]
        , playerTable dataTitle datumFunc players
        ]


playerTable : String -> (Player -> Int) -> List Player -> Html Msg
playerTable dataTitle datumFunc players =
    let
        headerRow =
            tr []
                [ th [] []
                , th [] [ text "Name" ]
                , th [] [ text dataTitle ]
                ]

        rows =
            List.indexedMap
                (\idx player ->
                    playerView idx datumFunc player
                )
                players
    in
    table [] (List.concat [ [ headerRow ], rows ])


playerView : Int -> (Player -> Int) -> Player -> Html Msg
playerView index getDatum player =
    tr []
        [ td [] [ text <| toString <| index + 1 ]
        , td [] [ text player.name ]
        , td [] [ text <| toString <| getDatum player ]
        ]



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


comparePlayers : (Player -> Int) -> Player -> Player -> Order
comparePlayers getComparable player1 player2 =
    compare (getComparable player1) (getComparable player2)


getScores : Model -> Player -> List Int
getScores model player =
    Dict.get player.id model.scoreData |> Maybe.withDefault []


getPersonalHighScore : Model -> Player -> Maybe Int
getPersonalHighScore model player =
    getScores model player |> List.maximum



--STYLES
--
