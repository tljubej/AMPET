module AMPET exposing (..)

import Audio
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
    , templateName : String
    }


firstBell : Time
firstBell =
    30 * Time.second


annoyingTime : Time
annoyingTime =
    -10 * Time.second


bellSound : String
bellSound =
    "sounds/bell.mp3"


annoyingSound : String
annoyingSound =
    "sounds/annoying.mp3"


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


type MsgEntry
    = Add String Time
    | Remove Id
    | Dismiss


type MsgChangeField
    = Name String
    | TemplateName String
    | Minutes String
    | Seconds String


type Msg
    = AddTemplate String
    | StartTimer
    | StopTimer
    | Tick Time
    | ChangeEntry MsgEntry
    | ChangeField MsgChangeField
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { entries = []
      , templates = []
      , lastId = 0
      , timerStarted = False
      , timer = 0
      , inputs = (Inputs "Tomislav" "1" "0" "Tomislav voli zelje")
      }
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


inputTemplateNameLens : Lens Inputs String
inputTemplateNameLens =
    Lens .templateName (\sn a -> { a | templateName = sn })


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
                if mins >= 0 && secs >= 0 then
                    toFloat mins * minute + toFloat secs * second |> Just
                else
                    Nothing

            _ ->
                Nothing


isTimeValid : Model -> Bool
isTimeValid model =
    case deserializeTime model.inputs.minutes model.inputs.seconds of
        Nothing ->
            False

        _ ->
            True


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEntry msgEntry ->
            updateEntry msgEntry model

        NoOp ->
            model ! []

        AddTemplate name ->
            { model | templates = model.templates ++ [ name ] } ! []

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
                        let
                            newTime =
                                time - Time.second

                            cmds =
                                if newTime == firstBell || newTime == 0 then
                                    [ Audio.playAudio bellSound ]
                                else if newTime <= annoyingTime then
                                    [ Audio.playAudio annoyingSound ]
                                else
                                    []
                        in
                            { newModel | entries = ( id, name, newTime ) :: rest } ! cmds

                    [] ->
                        newModel ! []

        ChangeField msgChangeField ->
            updateChangeField msgChangeField model


updateChangeField : MsgChangeField -> Model -> ( Model, Cmd Msg )
updateChangeField msgChangeField model =
    case msgChangeField of
        Name val ->
            set (modelInputsLens >=> inputNameLens) val model ! []

        Minutes val ->
            set (modelInputsLens >=> inputMinutesLens) val model ! []

        Seconds val ->
            set (modelInputsLens >=> inputSecondsLens) val model ! []

        TemplateName val ->
            set (modelInputsLens >=> inputTemplateNameLens) val model ! []


updateEntry : MsgEntry -> Model -> ( Model, Cmd Msg )
updateEntry msg model =
    case msg of
        Add name time ->
            let
                newId =
                    model.lastId + 1
            in
                { model | lastId = newId, entries = model.entries ++ [ ( model.lastId, name, time ) ] } ! []

        Remove id ->
            { model | entries = List.filter (\( oid, _, _ ) -> id /= oid) model.entries } ! []

        Dismiss ->
            case model.entries of
                hd :: rest ->
                    { model | entries = rest } ! []

                [] ->
                    model ! []


templateButtonStyle : Attribute a
templateButtonStyle =
    style [ ( "margin-right", "5px" ), ( "width", "100px" ), ( "padding", "0px 0px 0px 0px" ) ]


viewTemplate : String -> ( String, Html Msg )
viewTemplate name =
    ( name
    , tr []
        [ td [] [ text name ]
        , td []
            [ button [ ChangeEntry (Add name Time.minute) |> onClick, templateButtonStyle ] [ text "1 minute" ]
            , button [ ChangeEntry (Add name (Time.minute * 2)) |> onClick, templateButtonStyle ] [ text "2 minutes" ]
            , button [ ChangeEntry (Add name (Time.minute * 5)) |> onClick, templateButtonStyle ] [ text "5 minutes" ]
            , button [ ChangeEntry (Add name (Time.minute * 10)) |> onClick, templateButtonStyle ] [ text "10 minutes" ]
            ]
        ]
    )


viewEntries : Model -> List ( String, Html Msg )
viewEntries model =
    case model.entries of
        [] ->
            []

        e :: es ->
            (viewEntry model.timerStarted True) e :: List.map (viewEntry False False) es


viewEntry : Bool -> Bool -> Entry -> ( String, Html Msg )
viewEntry timerStarted first ( id, name, time ) =
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

        tdStyle =
            if first then
                style
                    [ ( "border", "1px solid" )
                    , ( "border-color"
                      , if timerStarted && inSeconds time < 30 then
                            "red"
                        else
                            "white"
                      )
                    ]
            else
                style []
    in
        ( toString id
        , tr []
            [ td [ tdStyle ] [ text name ]
            , td [ tdStyle ] [ text timeText ]
            , td [ tdStyle ] [ button [ ChangeEntry (Remove id) |> onClick, class "button button-clear" ] [ text "Remove" ] ]
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
                , button [ ChangeEntry Dismiss |> onClick, style [ ( "width", "200px" ) ] ] [ text "Finish Slot" ]
                ]
            , div [ class "row" ]
                [ label [ for "templateNameField", style [ ( "margin-right", "5px" ) ] ] [ text "Name" ]
                , input [ id "templateNameField", placeholder "Slaven", TemplateName >> ChangeField |> onInput, style [ ( "margin-right", "5px" ) ] ] []
                , button [ AddTemplate model.inputs.templateName |> onClick ] [ text "Add Template" ]
                ]
            , table []
                [ thead []
                    [ th [] [ text "Template name" ]
                    , th [] [ text "Preset times" ]
                    ]
                , Keyed.node "tbody"
                    []
                    (List.map viewTemplate model.templates)
                ]
            , div
                [ class "row"
                , style
                    [ ( "border"
                      , "1px solid"
                      )
                    , ( "border-color"
                      , if isTimeValid model then
                            "white"
                        else
                            "red"
                      )
                    ]
                ]
                [ label [ for "nameField", style [ ( "margin-right", "5px" ) ] ] [ text "Name" ]
                , input [ id "nameField", placeholder "Tomislav", Name >> ChangeField |> onInput, style [ ( "margin-right", "5px" ) ] ] []
                , label [ for "minutesField", style [ ( "margin-right", "5px" ) ] ] [ text "Minutes" ]
                , input [ id "minutesField", type_ "number", placeholder "1", Minutes >> ChangeField |> onInput, style [ ( "margin-right", "5px" ) ] ] []
                , label [ for "secondsField", style [ ( "margin-right", "5px" ) ] ] [ text "Seconds" ]
                , input [ id "secondsField", type_ "number", placeholder "0", Seconds >> ChangeField |> onInput, style [ ( "margin-right", "5px" ) ] ] []
                , button
                    [ onClick
                        (case deserializeTime model.inputs.minutes model.inputs.seconds of
                            Just time ->
                                ChangeEntry (Add model.inputs.name time)

                            Nothing ->
                                NoOp
                        )
                    , style [ ( "margin-right", "5px" ) ]
                    ]
                    [ text "Add" ]
                ]
            , h2 [] [ text "Time Slots:" ]
            ]
        , div [ class "column" ]
            [ table []
                [ thead []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Time Left" ]
                    ]
                , Keyed.node "tbody"
                    []
                    (viewEntries model)
                ]
            ]
        ]
