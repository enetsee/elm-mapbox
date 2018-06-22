module GeoJson.Feature exposing (Feature, encode)

{-|
@docs Feature, encode
-}

import GeoJson.Geometry as Geometry exposing (Geometry)
import Json.Encode as Encode


{-| -}
type alias Feature =
    { id : Maybe String
    , geometry : Maybe Geometry
    , properties : Maybe Encode.Value
    }


{-| -}
encode : Feature -> List ( String, Encode.Value )
encode feature =
    List.filterMap identity
        [ Just ( "type", Encode.string "Feature" )
        , Maybe.map (\id -> ( "id", Encode.string id )) feature.id
        , Maybe.map (\geometry -> ( "geometry", Encode.object <| Geometry.encode geometry )) feature.geometry
        , Maybe.map (\v -> ( "properties", v )) feature.properties
        ]
