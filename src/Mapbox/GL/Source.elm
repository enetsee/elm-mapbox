module Mapbox.GL.Source exposing (Source, vector, raster, rasterDEM, geoJson, geoJsonUrl, image, video, maxZoom, encode, reference)

{-|
@docs Source
@docs vector, raster, rasterDEM, geoJson,geoJsonUrl,image, video, reference
@docs maxZoom
@docs encode
-}

import GeoJson
import Json.Encode as Encode
import LngLat exposing (LngLat)
import LngLatBounds exposing (LngLatBounds)


{-| -}
type Source
    = Vector VectorSource
    | Raster RasterSource
    | RasterDEM RasterDEMSource
    | GeoJson GeoJsonSource
    | Image ImageSource
    | Video VideoSource
    | Ref String


{-| -}
encode : Source -> Encode.Value
encode source =
    case source of
        GeoJson attrs ->
            encodeGeoJsonSource attrs

        Vector attrs ->
            encodeVectorSource attrs

        Raster attrs ->
            encodeRasterSource attrs

        RasterDEM attrs ->
            encodeRasterDEMSource attrs

        Image attrs ->
            encodeImageSource attrs

        Video attrs ->
            encodeVideoSource attrs

        Ref id ->
            Encode.string id



-- API -------------------------------------------------------------------------


{-| -}
reference : String -> Source
reference id =
    Ref id


{-| -}
vector : String -> Source
vector url =
    Vector <| VectorSource url [] Nothing Nothing Nothing Nothing


{-| -}
raster : String -> Source
raster url =
    Raster <| RasterSource url [] Nothing Nothing Nothing Nothing Nothing Nothing


{-| -}
rasterDEM : String -> Source
rasterDEM url =
    RasterDEM <| RasterDEMSource url [] Nothing Nothing Nothing Nothing Nothing Nothing


{-| -}
geoJson : GeoJson.GeoJson -> Source
geoJson data =
    GeoJson <| GeoJsonSource (Literal data) Nothing Nothing Nothing Nothing Nothing


{-| -}
geoJsonUrl : String -> Source
geoJsonUrl url =
    GeoJson <| GeoJsonSource (URL url) Nothing Nothing Nothing Nothing Nothing


{-| -}
image : String -> Coordinates -> Source
image url coords =
    Image <| ImageSource url coords


{-| -}
video : List String -> Coordinates -> Source
video urls coords =
    Video <| VideoSource urls coords


{-| -}
maxZoom : Float -> Source -> Source
maxZoom zoom source =
    case source of
        Vector attrs ->
            maxZoom_ zoom attrs |> Vector

        Raster attrs ->
            maxZoom_ zoom attrs |> Raster

        RasterDEM attrs ->
            maxZoom_ zoom attrs |> RasterDEM

        GeoJson attrs ->
            maxZoom_ zoom attrs |> GeoJson

        _ ->
            source


maxZoom_ : a -> { c | maxZoom : b } -> { c | maxZoom : Maybe a }
maxZoom_ zoom source =
    { source | maxZoom = Just zoom }



-- Vector ----------------------------------------------------------------------


type alias VectorSource =
    { url : String
    , tiles : List String
    , bounds : Maybe LngLatBounds
    , minZoom : Maybe Float
    , maxZoom : Maybe Float
    , attribution : Maybe String
    }


encodeVectorSource : VectorSource -> Encode.Value
encodeVectorSource source =
    [ Just ( "type", Encode.string "vector" )
    , Just ( "url", Encode.string source.url )
    , (if List.isEmpty source.tiles then
        Nothing
       else
        Just ( "tiles", Encode.list <| List.map Encode.string source.tiles )
      )
    , Maybe.map (\b -> ( "bounds", encodeBounds b )) source.bounds
    , Maybe.map (\z -> ( "minzoom", Encode.float z )) source.minZoom
    , Maybe.map (\z -> ( "maxzoom", Encode.float z )) source.maxZoom
    , Maybe.map (\x -> ( "attribution", Encode.string x )) source.attribution
    ]
        |> List.filterMap identity
        |> Encode.object


encodeBounds :
    { c
        | ne : { a | lat : Float, lng : Float }
        , sw : { b | lat : Float, lng : Float }
    }
    -> Encode.Value
encodeBounds { sw, ne } =
    Encode.list
        [ Encode.float sw.lng, Encode.float sw.lat, Encode.float ne.lng, Encode.float ne.lat ]



-- Raster ----------------------------------------------------------------------


type alias RasterSource =
    { url : String
    , tiles : List String
    , bounds : Maybe LngLatBounds
    , minZoom : Maybe Float
    , maxZoom : Maybe Float
    , tileSize : Maybe Int
    , scheme : Maybe TileScheme
    , attribution : Maybe String
    }


encodeRasterSource : RasterSource -> Encode.Value
encodeRasterSource source =
    [ Just ( "type", Encode.string "raster" )
    , Just ( "url", Encode.string source.url )
    , (if List.isEmpty source.tiles then
        Nothing
       else
        Just ( "tiles", Encode.list <| List.map Encode.string source.tiles )
      )
    , Maybe.map (\b -> ( "bounds", encodeBounds b )) source.bounds
    , Maybe.map (\z -> ( "minzoom", Encode.float z )) source.minZoom
    , Maybe.map (\z -> ( "maxzoom", Encode.float z )) source.maxZoom
    , Maybe.map (\z -> ( "tileSize", Encode.int z )) source.tileSize
    , Maybe.map (\z -> ( "scheme", encodeTileScheme z )) source.scheme
    , Maybe.map (\x -> ( "attribution", Encode.string x )) source.attribution
    ]
        |> List.filterMap identity
        |> Encode.object


type TileScheme
    = XYZ
    | TMS


encodeTileScheme : TileScheme -> Encode.Value
encodeTileScheme scheme =
    case scheme of
        XYZ ->
            Encode.string "xyz"

        TMS ->
            Encode.string "tms"


{-| -}
xyz : TileScheme
xyz =
    XYZ


{-| -}
tms : TileScheme
tms =
    TMS



-- Raster DEM ------------------------------------------------------------------


type alias RasterDEMSource =
    { url : String
    , tiles : List String
    , bounds : Maybe LngLatBounds
    , minZoom : Maybe Float
    , maxZoom : Maybe Float
    , tileSize : Maybe Int
    , encoding : Maybe Encoding
    , attribution : Maybe String
    }


encodeRasterDEMSource : RasterDEMSource -> Encode.Value
encodeRasterDEMSource source =
    [ Just ( "type", Encode.string "raster-dem" )
    , Just ( "url", Encode.string source.url )
    , (if List.isEmpty source.tiles then
        Nothing
       else
        Just ( "tiles", Encode.list <| List.map Encode.string source.tiles )
      )
    , Maybe.map (\b -> ( "bounds", encodeBounds b )) source.bounds
    , Maybe.map (\z -> ( "minzoom", Encode.float z )) source.minZoom
    , Maybe.map (\z -> ( "maxzoom", Encode.float z )) source.maxZoom
    , Maybe.map (\z -> ( "tileSize", Encode.int z )) source.tileSize
    , Maybe.map (\z -> ( "encoding", encodeEncoding z )) source.encoding
    , Maybe.map (\x -> ( "attribution", Encode.string x )) source.attribution
    ]
        |> List.filterMap identity
        |> Encode.object


type Encoding
    = Terrarium
    | Mapbox


encodeEncoding : Encoding -> Encode.Value
encodeEncoding encoding =
    case encoding of
        Terrarium ->
            Encode.string "terrarium"

        Mapbox ->
            Encode.string "mapbox"


{-| -}
terrariumDEM : Encoding
terrariumDEM =
    Terrarium


{-| -}
mapboxDEM : Encoding
mapboxDEM =
    Mapbox



-- GeoJson ---------------------------------------------------------------------


type alias GeoJsonSource =
    { data : GeoJsonData
    , maxZoom : Maybe Float
    , buffer : Maybe Int
    , tolerance : Maybe Float
    , cluster : Maybe Clustering
    , lineMetrics : Maybe Bool
    }


type GeoJsonData
    = URL String
    | Literal GeoJson.GeoJson


encodeGeoJsonData : GeoJsonData -> Encode.Value
encodeGeoJsonData data =
    case data of
        URL url ->
            Encode.string url

        Literal geojson ->
            GeoJson.encode geojson


type Clustering
    = ClusterOn (Maybe Float) (Maybe Float)
    | ClusterOff


isClustered : Clustering -> Bool
isClustered c =
    case c of
        ClusterOn _ _ ->
            True

        _ ->
            False


clusterRadius : Clustering -> Maybe Float
clusterRadius c =
    case c of
        ClusterOn radius _ ->
            radius

        _ ->
            Nothing


clusterMaxZoom : Clustering -> Maybe Float
clusterMaxZoom c =
    case c of
        ClusterOn _ maxZoom ->
            maxZoom

        _ ->
            Nothing


encodeGeoJsonSource : GeoJsonSource -> Encode.Value
encodeGeoJsonSource source =
    [ Just ( "type", Encode.string "geojson" )
    , Just ( "data", encodeGeoJsonData source.data )
    , Maybe.map (\z -> ( "maxzoom", Encode.float z )) source.maxZoom
    , Maybe.map (\z -> ( "buffer", Encode.int z )) source.buffer
    , Maybe.map (\z -> ( "tolerance", Encode.float z )) source.tolerance
    , Maybe.map (\c -> ( "cluster", Encode.bool <| isClustered c )) source.cluster
    , Maybe.andThen (clusterRadius >> Maybe.map (\r -> ( "clusterRadius", Encode.float r ))) source.cluster
    , Maybe.andThen (clusterMaxZoom >> Maybe.map (\r -> ( "clusterMaxZoom", Encode.float r ))) source.cluster
    , Maybe.map (\c -> ( "lineMetrics", Encode.bool c )) source.lineMetrics
    ]
        |> List.filterMap identity
        |> Encode.object



-- Image -----------------------------------------------------------------------


type alias ImageSource =
    { url : String
    , coordinates : Coordinates
    }


encodeImageSource : ImageSource -> Encode.Value
encodeImageSource { url, coordinates } =
    Encode.object
        [ ( "type", Encode.string "image" )
        , ( "url", Encode.string url )
        , ( "coordinates", encodeCoordinates coordinates )
        ]


type alias Coordinates =
    { topLeft : LngLat, topRight : LngLat, bottomRight : LngLat, bottomLeft : LngLat }


encodeCoordinates : Coordinates -> Encode.Value
encodeCoordinates { topLeft, topRight, bottomRight, bottomLeft } =
    Encode.list
        [ Encode.list [ Encode.float topLeft.lng, Encode.float topLeft.lat ]
        , Encode.list [ Encode.float topRight.lng, Encode.float topRight.lat ]
        , Encode.list [ Encode.float bottomRight.lng, Encode.float bottomRight.lat ]
        , Encode.list [ Encode.float bottomLeft.lng, Encode.float bottomLeft.lat ]
        ]



-- Video -----------------------------------------------------------------------


type alias VideoSource =
    { urls : List String
    , coordinates : Coordinates
    }


encodeVideoSource : VideoSource -> Encode.Value
encodeVideoSource { urls, coordinates } =
    Encode.object
        [ ( "type", Encode.string "video" )
        , ( "urls", Encode.list <| List.map Encode.string urls )
        , ( "coordinates", encodeCoordinates coordinates )
        ]
