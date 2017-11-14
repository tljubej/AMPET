module AMPET exposing (..)

import List exposing (filter)
import Html exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second, minute)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Id =
    Int


type alias Inputs =
    { name : String
    , time : Time
    }


type alias Entry =
    ( Id, String, Time )


type alias Model =
    { entries : List Entry
    , lastId : Id
    , inputs : Inputs
    }


type Msg
    = AddEntry Entry
    | RemoveEntry Id
    | Start
    | Stop


init : ( Model, Cmd Msg )
init =
    ( Model [] 0 (Inputs "" 0), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newId =
            model.lastId + 1
    in
        case msg of
            AddEntry entry ->
                { model | lastId = newId, entries = model.entries ++ [ entry ] } ! []

            RemoveEntry id ->
                { model | entries = filter (\( oid, _, _ ) -> id /= oid) model.entries } ! []

            Start ->
                model ! []

            Stop ->
                model ! []


viewEntry : Entry -> Html Msg
viewEntry ( id, name, time ) =
    let
        minutes =
            round time // round (Time.minute)

        seconds =
            round time % round (60 * Time.second)
    in
        tr []
            [ td [] [ text (toString id) ]
            , td [] [ text name ]
            , td [] [ text (toString minutes ++ ":" ++ toString seconds) ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Enter new discussion slot:" ]
        , 
        , h2 [] [ text "Time Slots:" ]
        , table []
            [ thead []
                [ th [] [ text "Id" ]
                , th [] [ text "Name" ]
                , th [] [ text "Time Left" ]
                ]
            , tbody []
                (List.map (\entry -> viewEntry entry) model.entries)
            ]
        ]
