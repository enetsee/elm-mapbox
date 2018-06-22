module GeoJson.Position exposing (Position, encode)

{-|
@docs Position , encode
-}

import LngLat exposing (LngLat)
import Json.Encode as Encode


{-| -}
type alias Position =
    { lngLat : LngLat, altitude : Maybe Float }


{-| -}
encode : Position -> Encode.Value
encode { lngLat, altitude } =
    Encode.list <|
        List.filterMap identity
            [ Just <| Encode.float lngLat.lng
            , Just <| Encode.float lngLat.lat
            , Maybe.map Encode.float altitude
            ]
