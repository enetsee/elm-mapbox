module LngLatBounds exposing (LngLatBounds, fromPoint, union, unions, encode, decoder)

{-|
@docs LngLatBounds, fromPoint, union, unions,encode, decoder
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import LngLat exposing (LngLat)


{-| -}
type alias LngLatBounds =
    { sw : LngLat
    , ne : LngLat
    }


{-| -}
fromPoint : LngLat -> LngLatBounds
fromPoint lngLat =
    LngLatBounds lngLat lngLat


{-| -}
union : LngLatBounds -> LngLatBounds -> LngLatBounds
union b1 b2 =
    let
        sw =
            { lat = min b1.sw.lat b2.sw.lat, lng = min b1.sw.lng b2.sw.lng }

        ne =
            { lat = max b1.ne.lat b2.ne.lat, lng = max b1.ne.lng b2.ne.lng }
    in
        LngLatBounds sw ne


{-| -}
unions : List LngLatBounds -> Maybe LngLatBounds
unions bs =
    case bs of
        x :: xs ->
            List.foldl union x xs
                |> Just

        [] ->
            Nothing


{-| -}
decoder : Decoder LngLatBounds
decoder =
    Decode.constant LngLatBounds
        |> Decode.required "sw" LngLat.decoder
        |> Decode.required "ne" LngLat.decoder


{-| -}
encode : LngLatBounds -> Encode.Value
encode { sw, ne } =
    Encode.list [ LngLat.encode sw, LngLat.encode ne ]
