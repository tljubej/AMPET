module AMPET exposing (..)

import List
import Monocle.Lens exposing (..)
import Html exposing (..)
import Html.Keyed as Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Id =
    Int


type alias Inputs =
    { name : String
    , minutes : String
    , seconds : String
    }


type alias Entry =
    ( Id, String, Time )


type alias Model =
    { entries : List Entry
    , templates : List String
    , lastId : Id
    , timerStarted : Bool
    , timer : Time
    , inputs : Inputs
    }


type Msg
    = AddEntry String Time
    | NoOp
    | Tick Time
    | RemoveEntry Id
    | DismissEntry
    | StartTimer
    | StopTimer
    | ChangeNameField String
    | ChangeMinutesField String
    | ChangeSecondsField String


init : ( Model, Cmd Msg )
init =
    ( Model []
        [ "Toni"
        , "Tomislav"
        , "Slaven"
        , "Ivica"
        , "Mario"
        , "Ivan"
        , "Michael"
        , "Dietmar"
        ]
        0
        False
        0
        (Inputs "" "" "")
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerStarted then
        Time.every Time.second Tick
    else
        Sub.none


modelInputsLens : Lens Model Inputs
modelInputsLens =
    Lens .inputs (\sn a -> { a | inputs = sn })


inputNameLens : Lens Inputs String
inputNameLens =
    Lens .name (\sn a -> { a | name = sn })


inputMinutesLens : Lens Inputs String
inputMinutesLens =
    Lens .minutes (\sn a -> { a | minutes = sn })


inputSecondsLens : Lens Inputs String
inputSecondsLens =
    Lens .seconds (\sn a -> { a | seconds = sn })


set : Lens a b -> b -> a -> a
set lens val model =
    modify lens (\_ -> val) model


(>=>) : Lens a b -> Lens b c -> Lens a c
(>=>) lens1 lens2 =
    compose lens1 lens2


deserializeTime : String -> String -> Maybe Time
deserializeTime minutes seconds =
    let
        realMin =
            String.toInt minutes

        realSec =
            String.toInt seconds
    in
        case ( realMin, realSec ) of
            ( Ok mins, Ok secs ) ->
                toFloat mins * minute + toFloat secs * second |> Just

            _ ->
                Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        AddEntry name time ->
            let
                newId =
                    model.lastId + 1
            in
                { model | lastId = newId, entries = model.entries ++ [ ( model.lastId, name, time ) ] } ! []

        RemoveEntry id ->
            { model | entries = List.filter (\( oid, _, _ ) -> id /= oid) model.entries } ! []

        StartTimer ->
            { model | timerStarted = True } ! []

        StopTimer ->
            { model | timerStarted = False } ! []

        Tick _ ->
            let
                newModel =
                    { model | timer = model.timer + Time.second }
            in
                case model.entries of
                    ( id, name, time ) :: rest ->
                        { newModel | entries = ( id, name, time - Time.second ) :: rest } ! []

                    [] ->
                        newModel ! []

        DismissEntry ->
            case model.entries of
                hd :: rest ->
                    { model | entries = rest } ! []

                [] ->
                    model ! []

        ChangeNameField val ->
            set (modelInputsLens >=> inputNameLens) val model ! []

        ChangeMinutesField val ->
            set (modelInputsLens >=> inputMinutesLens) val model ! []

        ChangeSecondsField val ->
            set (modelInputsLens >=> inputSecondsLens) val model ! []


viewTemplate : String -> ( String, Html Msg )
viewTemplate name =
    ( name
    , tr []
        [ td [] [ text name ]
        , td []
            [ button [ AddEntry name Time.minute |> onClick ] [ text "1 minute" ]
            , button [ AddEntry name (Time.minute * 2) |> onClick ] [ text "2 minutes" ]
            , button [ AddEntry name (Time.minute * 5) |> onClick ] [ text "5 minutes" ]
            , button [ AddEntry name (Time.minute * 10) |> onClick ] [ text "10 minutes" ]
            ]
        ]
    )


viewEntry : Entry -> ( String, Html Msg )
viewEntry ( id, name, time ) =
    let
        minutes =
            if time >= 0 then
                floor (inMinutes time)
            else
                ceiling (inMinutes time) |> abs

        seconds =
            if time >= 0 then
                round (inSeconds time) - minutes * 60
            else
                minutes * 60 - round (inSeconds time) |> abs

        minSecText =
            String.padLeft 2 '0' (toString minutes) ++ ":" ++ String.padLeft 2 '0' (toString seconds)

        timeText =
            if time >= 0 then
                minSecText
            else
                "-" ++ minSecText
    in
        ( toString id
        , tr []
            [ td [] [ text name ]
            , td [] [ text timeText ]
            , td [] [ button [ RemoveEntry id |> onClick, class "button button-clear" ] [ text "Remove" ] ]
            ]
        )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "column" ]
            [ h2 [] [ text "Enter new discussion slot:" ]
            , div [ class "row" ]
                [ button [ onClick StartTimer, style [ ( "margin-right", "5px" ) ] ] [ text "Start Timer" ]
                , button [ onClick StopTimer, class "button button-outline", style [ ( "margin-right", "15px" ) ] ] [ text "Stop Timer" ]
                , button [ onClick DismissEntry, style [ ( "width", "200px" ) ] ] [ text "Finish Slot" ]
                ]
            , div [ class "row" ]
                [ label [ for "nameField", style [ ( "margin-right", "5px" ) ] ] [ text "Name" ]
                , input [ id "nameField", placeholder "Slaven", onInput ChangeNameField, style [ ( "margin-right", "5px" ) ] ] []
                , label [ for "minutesField", style [ ( "margin-right", "5px" ) ] ] [ text "Minutes" ]
                , input [ id "minutesField", type_ "number", placeholder "2", onInput ChangeMinutesField, style [ ( "margin-right", "5px" ) ] ] []
                , label [ for "secondsField", style [ ( "margin-right", "5px" ) ] ] [ text "Seconds" ]
                , input [ id "secondsField", type_ "number", placeholder "0", onInput ChangeSecondsField, style [ ( "margin-right", "5px" ) ] ] []
                , button
                    [ onClick
                        (case deserializeTime model.inputs.minutes model.inputs.seconds of
                            Just time ->
                                AddEntry model.inputs.name time

                            Nothing ->
                                NoOp
                        )
                    , style [ ( "margin-right", "5px" ) ]
                    ]
                    [ text "Add" ]
                ]
            , h2 [] [ text "Time Slots:" ]
            ]
        , div [class "column"] [table []
            [ thead []
                [ th [] [ text "Name" ]
                , th [] [ text "Time Left" ]
                ]
            , Keyed.node "tbody"
                []
                (List.map (\entry -> viewEntry entry) model.entries)
            ]]
        ]
