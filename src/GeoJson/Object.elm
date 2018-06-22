module GeoJson.Object exposing (Object, encode, geometry, featureCollection, featureObject)

{-|
@docs Object,encode
@docs geometry, featureCollection, featureObject
-}

import GeoJson.Feature as Feature exposing (Feature)
import GeoJson.Geometry as Geometry exposing (Geometry)
import Json.Encode as Encode


{-| -}
type Object
    = Geometry Geometry
    | FeatureCollection (List Feature)
    | FeatureObject Feature


{-| -}
geometry : Geometry -> Object
geometry =
    Geometry


{-| -}
featureCollection : List Feature -> Object
featureCollection =
    FeatureCollection


{-| -}
featureObject : Feature -> Object
featureObject =
    FeatureObject


{-| -}
encode : Object -> List ( String, Encode.Value )
encode obj =
    case obj of
        FeatureObject feature ->
            Feature.encode feature

        Geometry geometry ->
            Geometry.encode geometry

        FeatureCollection features ->
            [ ( "type", Encode.string "FeatureCollection" )
            , ( "features", Encode.list <| List.map (Feature.encode >> Encode.object) <| features )
            ]
