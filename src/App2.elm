module App exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


--MODEL & TYPES


type alias Model =
    { people : List Person
    , activeFilter : FilterType
    }


type alias Person =
    { id : Int
    , name : String
    , age : Int
    }


type alias FilterFunc =
    Person -> Bool


type FilterType
    = All
    | Children
    | Adults


filter : FilterType -> FilterFunc
filter filterType =
    case filterType of
        All ->
            \_ -> True

        Children ->
            \person -> person.age < 18

        Adults ->
            \person -> person.age >= 18



--INIT


init : ( Model, Cmd Msg )
init =
    ( { people =
            [ Person 0 "Houreidja" 24
            , Person 1 "Elana" 28
            , Person 2 "Josh" 18
            , Person 3 "Yosef" 4
            , Person 4 "Sana" 17
            , Person 5 "Tina" 56
            ]
      , activeFilter = All
      }
    , Cmd.none
    )



--UPDATE


type Msg
    = SetFilter FilterType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFilter filterType ->
            ( { model | activeFilter = filterType }
            , Cmd.none
            )



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "People" ]
        , personList model
        , div []
            [ button
                [ onClick (SetFilter Adults) ]
                [ text "Adults" ]
            , button
                [ onClick (SetFilter Children) ]
                [ text "Children" ]
            , button
                [ onClick (SetFilter All) ]
                [ text "All" ]
            ]
        ]


personList : Model -> Html Msg
personList model =
    ul []
        (List.map (personView model) model.people)


personView : Model -> Person -> Html Msg
personView model person =
    let
        attrs =
            case filter model.activeFilter person of
                True ->
                    []

                False ->
                    [ filtered ]
    in
    li attrs [ text (displayText person) ]


displayText : Person -> String
displayText person =
    person.name ++ " - " ++ toString person.age



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--LOGIC
--STYLES


filtered : Attribute Msg
filtered =
    style
        [ ( "color", "lightgray" )
        ]
