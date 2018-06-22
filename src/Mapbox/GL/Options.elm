module Mapbox.GL.Options
    exposing
        ( Options
        , options
        , encode
        , LogoPosition(..)
        , encodeLogoPosition
        , attributionControl
        , doubleClickZoom
        )

{-|
@docs Options,options
@docs LogoPosition
@docs attributionControl, doubleClickZoom
@docs encode, encodeLogoPosition
-}

import Json.Encode as Encode
import LngLatBounds as LngLatBounds exposing (LngLatBounds)
import LngLat as LngLat exposing (LngLat)


-- API -------------------------------------------------------------------------


{-| -}
attributionControl : Bool -> Options -> Options
attributionControl onOrOff options =
    { options | attributionControl = Just onOrOff }


{-| -}
doubleClickZoom : Bool -> Options -> Options
doubleClickZoom onOrOff options =
    { options | doubleClickZoom = Just onOrOff }



-- Types -----------------------------------------------------------------------


{-| -}
type alias Options =
    { container : String
    , center : LngLat
    , zoom : Int
    , bearing : Float
    , pitch : Float
    , minZoom : Maybe Int
    , maxZoom : Maybe Int
    , style : String
    , hash : Maybe Bool
    , interactive : Maybe Bool
    , bearingSnap : Maybe Int
    , pitchWithRotate : Maybe Bool
    , attributionControl : Maybe Bool
    , logoPosition : Maybe LogoPosition
    , failIfMajorPerformanceCaveat : Maybe Bool
    , preserveDrawingBuffer : Maybe Bool
    , refreshExpiredTiles : Maybe Bool
    , maxBounds : Maybe LngLatBounds
    , scrollZoom : Maybe Bool
    , boxZoom : Maybe Bool
    , dragRotate : Maybe Bool
    , dragPan : Maybe Bool
    , keyboard : Maybe Bool
    , doubleClickZoom : Maybe Bool
    , touchZoomRotate : Maybe Bool
    , trackResize : Maybe Bool
    , renderWorldCopies : Maybe Bool
    , maxTileCache : Maybe Int
    }


{-| -}
options :
    { a
        | container : String
        , center : LngLat
        , zoom : Int
        , bearing : Float
        , pitch : Float
        , style : String
    }
    -> Options
options { container, center, zoom, bearing, pitch, style } =
    { container = container
    , center = center
    , zoom = zoom
    , bearing = bearing
    , pitch = pitch
    , minZoom = Nothing
    , maxZoom = Nothing
    , style = style
    , hash = Nothing
    , interactive = Nothing
    , bearingSnap = Nothing
    , pitchWithRotate = Nothing
    , attributionControl = Nothing
    , logoPosition = Nothing
    , failIfMajorPerformanceCaveat = Nothing
    , preserveDrawingBuffer = Nothing
    , refreshExpiredTiles = Nothing
    , maxBounds = Nothing
    , scrollZoom = Nothing
    , boxZoom = Nothing
    , dragRotate = Nothing
    , dragPan = Nothing
    , keyboard = Nothing
    , doubleClickZoom = Nothing
    , touchZoomRotate = Nothing
    , trackResize = Nothing
    , renderWorldCopies = Nothing
    , maxTileCache = Nothing
    }


{-| -}
encode : Options -> Encode.Value
encode options =
    Encode.object <|
        List.filterMap identity
            [ Just ( "container", Encode.string options.container )
            , Just ( "center", LngLat.encode options.center )
            , Just ( "zoom", Encode.int options.zoom )
            , Just ( "bearing", Encode.float options.bearing )
            , Just ( "pitch", Encode.float options.pitch )
            , encodeMaybe "minZoom" Encode.int options.minZoom
            , encodeMaybe "maxZoom" Encode.int options.maxZoom
            , Just ( "style", Encode.string options.style )
            , encodeMaybe "hash" Encode.bool options.hash
            , encodeMaybe "interactive" Encode.bool options.interactive
            , encodeMaybe "bearingSnap" Encode.int options.bearingSnap
            , encodeMaybe "pitchWithRotate" Encode.bool options.pitchWithRotate
            , encodeMaybe "attributionControl" Encode.bool options.attributionControl
            , encodeMaybe "logoPosition" encodeLogoPosition options.logoPosition
            , encodeMaybe "failIfMajorPerformanceCaveat" Encode.bool options.failIfMajorPerformanceCaveat
            , encodeMaybe "preserveDrawingBuffer" Encode.bool options.preserveDrawingBuffer
            , encodeMaybe "refreshExpiredTiles" Encode.bool options.refreshExpiredTiles
            , encodeMaybe "maxBounds" LngLatBounds.encode options.maxBounds
            , encodeMaybe "scrollZoom" Encode.bool options.scrollZoom
            , encodeMaybe "boxZoom" Encode.bool options.boxZoom
            , encodeMaybe "dragRotate" Encode.bool options.dragRotate
            , encodeMaybe "dragPan" Encode.bool options.dragPan
            , encodeMaybe "keyboard" Encode.bool options.keyboard
            , encodeMaybe "doubleClickZoom" Encode.bool options.doubleClickZoom
            , encodeMaybe "touchZoomRotate" Encode.bool options.touchZoomRotate
            , encodeMaybe "trackResize" Encode.bool options.trackResize
            , encodeMaybe "renderWorldCopies" Encode.bool options.renderWorldCopies
            , encodeMaybe "maxTileCache" Encode.int options.maxTileCache
            ]


encodeMaybe : a -> (b -> c) -> Maybe b -> Maybe ( a, c )
encodeMaybe name encoder maybeVal =
    Maybe.map (\x -> ( name, encoder x )) maybeVal


{-| -}
type LogoPosition
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


{-| -}
logoPositionToString : LogoPosition -> String
logoPositionToString pos =
    case pos of
        TopLeft ->
            "top-left"

        TopRight ->
            "top-right"

        BottomLeft ->
            "bottom-left"

        BottomRight ->
            "bottom-right"


{-| -}
encodeLogoPosition : LogoPosition -> Encode.Value
encodeLogoPosition =
    logoPositionToString >> Encode.string
