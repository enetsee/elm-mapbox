port module Mapbox.GL.Ports exposing (OutwardMsg(..), IncomingMsg(..), send, receive)

{-|
@docs OutwardMsg , send
@docs IncomingMsg , receive
-}

import Json.Decode as Decode
import Json.Encode as Encode
import Mapbox.GL.Source exposing (Source)
import Mapbox.GL.Layer exposing (Layer)
import Mapbox.GL.Options exposing (Options)
import Mapbox.Token exposing (Token)
import LngLat exposing (LngLat)
import LngLatBounds exposing (LngLatBounds)


{-| -}
port outgoing : JSMsg -> Cmd msg


{-| -}
port incoming : (JSMsg -> msg) -> Sub msg


{-| -}
type alias JSMsg =
    { tag : String, data : Encode.Value }


{-| -}
type OutwardMsg
    = LogError String
    | SetToken Token
    | CreateMap Options
    | RemoveMap
    | FitBounds LngLatBounds
    | AddLayer Layer
    | RemoveLayer String
    | AddSource String Source
    | RemoveSource String


{-| -}
type IncomingMsg
    = NoOp
    | ClickedMap LngLat


{-| -}
send : OutwardMsg -> Cmd msg
send msg =
    let
        jsdata =
            case msg of
                LogError err ->
                    JSMsg "LogError" Encode.null

                SetToken token ->
                    JSMsg "SetToken" <| Mapbox.Token.encode token

                CreateMap options ->
                    JSMsg "CreateMap" <| Mapbox.GL.Options.encode options

                RemoveMap ->
                    JSMsg "RemoveMap" Encode.null

                FitBounds bounds ->
                    JSMsg "FitBounds" <| LngLatBounds.encode bounds

                AddLayer layer ->
                    JSMsg "AddLayer" <| Mapbox.GL.Layer.encode layer

                RemoveLayer id ->
                    JSMsg "RemoveLayer" <| Encode.string id

                AddSource name source ->
                    JSMsg "AddSource" <|
                        Encode.object
                            [ ( "id", Encode.string name )
                            , ( "source", Mapbox.GL.Source.encode source )
                            ]

                RemoveSource id ->
                    JSMsg "RemoveSource" <| Encode.string id
    in
        outgoing jsdata


{-| -}
receive : (IncomingMsg -> a) -> (String -> a) -> Sub a
receive toMsg onError =
    incoming <|
        \jsmsg ->
            case jsmsg.tag of
                "NoOp" ->
                    toMsg NoOp

                "ClickedMap" ->
                    case Decode.decodeValue LngLat.decoder jsmsg.data of
                        Ok latlng ->
                            toMsg <| ClickedMap latlng

                        Err msg ->
                            onError msg

                _ ->
                    onError <| "Unexpected message received: `" ++ toString jsmsg ++ "`."
