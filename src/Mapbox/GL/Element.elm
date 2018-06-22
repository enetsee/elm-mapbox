module Mapbox.GL.Element
    exposing
        ( mapboxGL
        , accessToken
        , fromOptions
        , center
        , zoom
        , bearing
        , pitch
        , minZoom
        , maxZoom
        , mapStyle
        , hash
        , interactive
        , bearingSnap
        , pitchWithRotate
        , attributionControl
        , logoPosition
        , failIfMajorPerformanceCaveat
        , preserveDrawingBuffer
        , refreshExpiredTiles
        , maxBounds
        , scrollZoom
        , boxZoom
        , dragRotate
        , dragPan
        , keyboard
        , doubleClickZoom
        , touchZoomRotate
        , trackResize
        , renderWorldCopies
        , maxTileCache
        , sources
        , layers
        )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A exposing (property)
import Json.Encode as Encode
import LngLat exposing (LngLat)
import LngLatBounds exposing (LngLatBounds)
import Mapbox.GL.Options as Options exposing (Options, LogoPosition)
import Mapbox.Token as Token exposing (Token)
import Mapbox.GL.Source as Source exposing (Source)
import Mapbox.GL.Layer as Layer exposing (Layer)


mapboxGL : List (Attribute msg) -> List (Html msg) -> Html msg
mapboxGL attrs children =
    H.node "mapbox-gl" attrs children


fromOptions : Token -> Options -> List (Html msg) -> Html msg
fromOptions token options =
    [ Just <| accessToken token
    , Just <| center options.center
    , Just <| zoom options.zoom
    , Just <| bearing options.bearing
    , Just <| pitch options.pitch
    , Just <| mapStyle options.style
    , Maybe.map minZoom options.minZoom
    , Maybe.map maxZoom options.maxZoom
    , Maybe.map hash options.hash
    , Maybe.map interactive options.interactive
    , Maybe.map bearingSnap options.bearingSnap
    , Maybe.map pitchWithRotate options.pitchWithRotate
    , Maybe.map attributionControl options.attributionControl
    , Maybe.map logoPosition options.logoPosition
    , Maybe.map failIfMajorPerformanceCaveat options.failIfMajorPerformanceCaveat
    , Maybe.map preserveDrawingBuffer options.preserveDrawingBuffer
    , Maybe.map refreshExpiredTiles options.refreshExpiredTiles
    , Maybe.map maxBounds options.maxBounds
    , Maybe.map scrollZoom options.scrollZoom
    , Maybe.map boxZoom options.boxZoom
    , Maybe.map dragRotate options.dragRotate
    , Maybe.map dragPan options.dragPan
    , Maybe.map keyboard options.keyboard
    , Maybe.map doubleClickZoom options.doubleClickZoom
    , Maybe.map touchZoomRotate options.touchZoomRotate
    , Maybe.map trackResize options.trackResize
    , Maybe.map renderWorldCopies options.renderWorldCopies
    , Maybe.map maxTileCache options.maxTileCache
    ]
        |> List.filterMap identity
        |> mapboxGL


sources : List ( String, Source ) -> Attribute msg
sources namedSources =
    property "sources" <|
        Encode.list <|
            List.map
                (\( name, source ) ->
                    Encode.object
                        [ ( "name", Encode.string name )
                        , ( "source", Source.encode source )
                        ]
                )
                namedSources


layers : List Layer -> Attribute msg
layers ls =
    property "layers" <|
        Encode.list <|
            List.map Layer.encode ls


accessToken : Token -> Attribute msg
accessToken token =
    property "accessToken" (Token.encode token)


center : LngLat -> Attribute msg
center lnglat =
    property "center" (LngLat.encode lnglat)


zoom : Int -> Attribute msg
zoom zoom =
    property "zoom" (Encode.int zoom)


bearing : Float -> Attribute msg
bearing value =
    property "bearing" (Encode.float value)


pitch : Float -> Attribute msg
pitch value =
    property "pitch" (Encode.float value)


minZoom : Int -> Attribute msg
minZoom value =
    property "minZoom" (Encode.int value)


maxZoom : Int -> Attribute msg
maxZoom value =
    property "maxZoom" (Encode.int value)


mapStyle : String -> Attribute msg
mapStyle value =
    property "mapStyle" (Encode.string value)


hash : Bool -> Attribute msg
hash value =
    property "hash" (Encode.bool value)


interactive : Bool -> Attribute msg
interactive value =
    property "interactive" (Encode.bool value)


bearingSnap : Int -> Attribute msg
bearingSnap value =
    property "bearingSnap" (Encode.int value)


pitchWithRotate : Bool -> Attribute msg
pitchWithRotate value =
    property "pitchWithRotate" (Encode.bool value)


attributionControl : Bool -> Attribute msg
attributionControl value =
    property "attributionControl" (Encode.bool value)


logoPosition : LogoPosition -> Attribute msg
logoPosition pos =
    property "logoPosition" (Options.encodeLogoPosition pos)


failIfMajorPerformanceCaveat : Bool -> Attribute msg
failIfMajorPerformanceCaveat value =
    property "failIfMajorPerformanceCaveat" (Encode.bool value)


preserveDrawingBuffer : Bool -> Attribute msg
preserveDrawingBuffer value =
    property "preserveDrawingBuffer" (Encode.bool value)


refreshExpiredTiles : Bool -> Attribute msg
refreshExpiredTiles value =
    property "refreshExpiredTiles" (Encode.bool value)


maxBounds : LngLatBounds -> Attribute msg
maxBounds bounds =
    property "bounds" (LngLatBounds.encode bounds)


scrollZoom : Bool -> Attribute msg
scrollZoom value =
    property "scrollZoom" (Encode.bool value)


boxZoom : Bool -> Attribute msg
boxZoom value =
    property "boxZoom" (Encode.bool value)


dragRotate : Bool -> Attribute msg
dragRotate value =
    property "dragRotate" (Encode.bool value)


dragPan : Bool -> Attribute msg
dragPan value =
    property "dragPan" (Encode.bool value)


keyboard : Bool -> Attribute msg
keyboard value =
    property "keyboard" (Encode.bool value)


doubleClickZoom : Bool -> Attribute msg
doubleClickZoom value =
    property "doubleClickZoom" (Encode.bool value)


touchZoomRotate : Bool -> Attribute msg
touchZoomRotate value =
    property "touchZoomRotate" (Encode.bool value)


trackResize : Bool -> Attribute msg
trackResize value =
    property "trackResize" (Encode.bool value)


renderWorldCopies : Bool -> Attribute msg
renderWorldCopies value =
    property "renderWorldCopies" (Encode.bool value)


maxTileCache : Int -> Attribute msg
maxTileCache value =
    property "maxTileCache" (Encode.int value)
