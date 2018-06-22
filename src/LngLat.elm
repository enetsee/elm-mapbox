module LngLat exposing (LngLat, encode, decoder)

{-|
@docs LngLat, encode, decoder
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode


{-| -}
type alias LngLat =
    { lng : Float
    , lat : Float
    }


{-| -}
decoder : Decoder LngLat
decoder =
    Decode.constant LngLat
        |> Decode.required "lng" Decode.float
        |> Decode.required "lat" Decode.float


{-| -}
encode : LngLat -> Encode.Value
encode { lng, lat } =
    Encode.list
        [ Encode.float lng
        , Encode.float lat
        ]
