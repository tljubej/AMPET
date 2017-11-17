port module Audio exposing (..)

port playAudio : String -> Cmd msg

port killSounds : () -> Cmd msg