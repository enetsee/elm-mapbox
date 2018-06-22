module GeoJson
    exposing
        ( GeoJson
        , encode
        )

{-|
@docs GeoJson, encode
-}

import GeoJson.Object as Object exposing (Object)
import Json.Encode as Encode
import LngLatBounds exposing (LngLatBounds)


{-| -}
type alias GeoJson =
    { object : Object
    , boundingBox : Maybe LngLatBounds
    }


{-| -}
encode : GeoJson -> Encode.Value
encode { object, boundingBox } =
    let
        obj =
            Object.encode object
    in
        case boundingBox of
            Just bb ->
                Encode.object <|
                    (encodeBoundingBox bb)
                        :: obj

            _ ->
                Encode.object obj


{-| GeoJson encodes the bounding box as an array of floats rather than an object
-}
encodeBoundingBox : LngLatBounds -> ( String, Encode.Value )
encodeBoundingBox { sw, ne } =
    ( "bbox", Encode.list [ Encode.float sw.lng, Encode.float sw.lat, Encode.float ne.lng, Encode.float ne.lat ] )
