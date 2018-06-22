module GeoJson.Geometry
    exposing
        ( Geometry
        , encode
        , point
        , multiPoint
        , lineString
        , multiLineString
        , polygon
        , multiPolygon
        , geometryCollection
        )

{-|
@docs Geometry, encode
@docs point, multiPoint, lineString, multiLineString, polygon,multiPolygon, geometryCollection
-}

import GeoJson.Position as Position exposing (Position)
import Json.Encode as Encode


{-| -}
type Geometry
    = Point Position
    | MultiPoint (List Position)
    | LineString (List Position)
    | MultiLineString (List (List Position))
    | Polygon (List (List Position))
    | MultiPolygon (List (List (List Position)))
    | GeometryCollection (List Geometry)


{-| -}
point : Position -> Geometry
point =
    Point


{-| -}
multiPoint : List Position -> Geometry
multiPoint =
    MultiPoint


{-| -}
lineString : List Position -> Geometry
lineString =
    LineString


{-| -}
multiLineString : List (List Position) -> Geometry
multiLineString =
    MultiLineString


{-| -}
polygon : List (List Position) -> Geometry
polygon =
    Polygon


{-| -}
multiPolygon : List (List (List Position)) -> Geometry
multiPolygon =
    MultiPolygon


{-| -}
geometryCollection : List Geometry -> Geometry
geometryCollection =
    GeometryCollection


{-| -}
encode : Geometry -> List ( String, Encode.Value )
encode geom =
    case geom of
        Point data ->
            [ ( "type", Encode.string "Point" )
            , ( "coordinates", Position.encode data )
            ]

        MultiPoint data ->
            [ ( "type", Encode.string "MultiPoint" )
            , ( "coordinates", data |> List.map Position.encode |> Encode.list )
            ]

        LineString data ->
            [ ( "type", Encode.string "LineString" )
            , ( "coordinates", data |> List.map Position.encode |> Encode.list )
            ]

        MultiLineString data ->
            [ ( "type", Encode.string "MultiLineString" )
            , ( "coordinates", data |> List.map (List.map Position.encode >> Encode.list) |> Encode.list )
            ]

        Polygon data ->
            [ ( "type", Encode.string "Polygon" )
            , ( "coordinates", data |> List.map (List.map Position.encode >> Encode.list) |> Encode.list )
            ]

        MultiPolygon data ->
            [ ( "type", Encode.string "MultiPolygon" )
            , ( "coordinates", data |> List.map (List.map (List.map Position.encode >> Encode.list) >> Encode.list) |> Encode.list )
            ]

        GeometryCollection data ->
            [ ( "type", Encode.string "GeometryCollection" )
            , ( "geometries", data |> List.map (encode >> Encode.object) |> Encode.list )
            ]
