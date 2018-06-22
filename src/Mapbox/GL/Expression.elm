module Mapbox.GL.Expression
    exposing
        ( Expression
        , encode
        , at
        , get
        , get_
        , has
        , length
        , stringLength
        , not
        , and
        , or
        , eq
        , neq
        , gt
        , lt
        , lte
        , gte
        , all
        , any
        , add
        , sub
        , mult
        , div
        , mod
        , pow
        , neg
        , abs
        , acos
        , asin
        , atan
        , ceil
        , cos
        , floor
        , ln
        , ln2
        , log10
        , round
        , sin
        , sqrt
        , tan
        , min
        , max
        , interpolate
        , linear
        , exponential
        , cubicBezier
        , step
        , concat
        , downcase
        , upcase
        , zoom
        , heatmapDensity
        , e
        , pi
        , number
        , string
        , color
        , bool
        , rgb
        , rgba
        , toRgba
        , letIn
        , letIns
        )

{-|
@docs Expression, encode

## Feature data
@docs at, get, get_, has, length

## Decision
@docs not,and,or,eq,neq,gt,lt,lte,gte,all,any

## Math
@docs add, sub, mult, div, mod, pow, neg, abs, acos, asin,atan, ceil, cos, floor, ln, ln2, log10, round,sin,sqrt,tan,min,max

## Ramps, scales, curves
@docs interpolate ,linear, exponential , cubicBezier ,step

## String
@docs concat, stringLength, downcase, upcase

## Color
@docs rgb, rgba, toRgba

## Constants
@docs e, pi

## Literals

@docs number, string, bool, color

## Other
@docs zoom, heatmapDensity, letIn , letIns


-}

import Color exposing (Color)
import Json.Encode as Encode
import Mapbox.GL.Expression.Internal as Internal exposing (Expression(E), Expr(..), Lit(..), BinOp(..), UnOp(..), Interpolation(..))


{-| -}
type alias Expression a =
    Internal.Expression a


{-| -}
encode : Expression a -> Encode.Value
encode (E expr) =
    Internal.encode expr


type GeometryTy
    = Point
    | MultiPoint
    | LineString
    | MultiLineString
    | Polygon
    | MultiPolygon



-- Binding ---------------------------------------------------------------------


{-| -}
letIn : String -> Expr -> Expr -> Expr
letIn var binding body =
    LetIn [ ( var, binding ) ] body


{-| -}
letIns : List ( String, Expr ) -> Expr -> Expr
letIns bindings body =
    LetIn bindings body



-- Constants -------------------------------------------------------------------


{-| -}
e : Expression Float
e =
    E ConstantE


{-| -}
pi : Expression Float
pi =
    E ConstantPi


{-| -}
zoom : Expression Float
zoom =
    E Zoom


{-| -}
heatmapDensity : Expression Float
heatmapDensity =
    E HeatmapDensity



-- Literals --------------------------------------------------------------------


{-| -}
number : Float -> Expression Float
number n =
    E <| Literal <| LitNumber n


{-| -}
string : String -> Expression String
string str =
    E <| Literal <| LitString str


{-| -}
bool : Bool -> Expression Bool
bool b =
    E <| Literal <| LitBool b


{-| -}
color : Color -> Expression Color
color c =
    E <| Literal <| LitColor c



-- Math ------------------------------------------------------------------------


{-| -}
add : Expression Float -> Expression Float -> Expression Float
add (E a) (E b) =
    E <| BinOp Add a b


{-| -}
sub : Expression Float -> Expression Float -> Expression Float
sub (E a) (E b) =
    E <| BinOp Sub a b


{-| -}
mult : Expression Float -> Expression Float -> Expression Float
mult (E a) (E b) =
    E <| BinOp Mult a b


{-| -}
div : Expression Float -> Expression Float -> Expression Float
div (E a) (E b) =
    E <| BinOp Div a b


{-| -}
mod : Expression Float -> Expression Float -> Expression Float
mod (E a) (E b) =
    E <| BinOp Mod a b


{-| -}
pow : Expression Float -> Expression Float -> Expression Float
pow (E a) (E b) =
    E <| BinOp Pow a b


{-| -}
neg : Expression Float -> Expression Float
neg (E expr) =
    E <| UnOp Neg expr


{-| -}
abs : Expression Float -> Expression Float
abs (E expr) =
    E <| UnOp Abs expr


{-| -}
acos : Expression Float -> Expression Float
acos (E expr) =
    E <| UnOp Acos expr


{-| -}
asin : Expression Float -> Expression Float
asin (E expr) =
    E <| UnOp Asin expr


{-| -}
atan : Expression Float -> Expression Float
atan (E expr) =
    E <| UnOp Atan expr


{-| -}
ceil : Expression Float -> Expression Float
ceil (E expr) =
    E <| UnOp Ceil expr


{-| -}
cos : Expression Float -> Expression Float
cos (E expr) =
    E <| UnOp Cos expr


{-| -}
floor : Expression Float -> Expression Float
floor (E expr) =
    E <| UnOp Floor expr


{-| -}
ln : Expression Float -> Expression Float
ln (E expr) =
    E <| UnOp Ln expr


{-| -}
ln2 : Expression Float -> Expression Float
ln2 (E expr) =
    E <| UnOp Ln2 expr


{-| -}
log10 : Expression Float -> Expression Float
log10 (E expr) =
    E <| UnOp Log10 expr


{-| -}
round : Expression Float -> Expression Float
round (E expr) =
    E <| UnOp Round expr


{-| -}
sin : Expression Float -> Expression Float
sin (E expr) =
    E <| UnOp Sin expr


{-| -}
sqrt : Expression Float -> Expression Float
sqrt (E expr) =
    E <| UnOp Sqrt expr


{-| -}
tan : Expression Float -> Expression Float
tan (E expr) =
    E <| UnOp Tan expr


{-| -}
min : List (Expression Float) -> Expression Float
min bs =
    E <| Min <| List.map Internal.unE bs


{-| -}
max : List (Expression Float) -> Expression Float
max bs =
    E <| Max <| List.map Internal.unE bs



-- Interpolation ---------------------------------------------------------------


{-| -}
linear : Interpolation
linear =
    Linear


{-| -}
exponential : Float -> Interpolation
exponential base =
    Exponential base


{-| -}
cubicBezier : Float -> Float -> Float -> Float -> Interpolation
cubicBezier x1 y1 x2 y2 =
    CubicBezier x1 y1 x2 y2


{-| -}
interpolate : Interpolation -> Expression Float -> List ( Expression Float, Expression a ) -> Expression a
interpolate method (E input) stops =
    E <| Interpolate method input <| List.map (\( E x, E y ) -> ( x, y )) stops


{-| -}
step : Expression Float -> Expression a -> List ( Expression Float, Expression a ) -> Expression a
step (E input) (E output0) stops =
    E <| Step input output0 <| List.map (\( E x, E y ) -> ( x, y )) stops



-- Feature data ----------------------------------------------------------------


{-| -}
geometryType : Expression GeometryTy
geometryType =
    E GeometryType


{-| -}
properties : Expression a
properties =
    E Properties


{-| -}
id : Expression String
id =
    E Id



-- Lookup ----------------------------------------------------------------------


{-| -}
at : Int -> Expression (List a) -> Expression a
at n (E array) =
    E <| At n <| array


{-| -}
get : String -> Expression a -> Expression b
get field (E object) =
    E <| Get field (Just object)


{-| -}
get_ : String -> Expression a
get_ field =
    E <| Get field Nothing


{-| -}
has : String -> (a -> b) -> Expression a -> Expression b
has field getter (E object) =
    E <| Has field (Just object)


{-| -}
length : Expression (List a) -> Expression Float
length (E expr) =
    E <| Length expr


{-| -}
stringLength : Expression String -> Expression Float
stringLength (E expr) =
    E <| Length expr



-- Logical operations ----------------------------------------------------------


{-| Logical negation. Returns true if the input is false, and false if the input is true.
-}
not : Expression Bool -> Expression Bool
not (E a) =
    E <| UnOp Not a


{-| -}
and : Expression Bool -> Expression Bool -> Expression Bool
and (E a) (E b) =
    E <| All [ a, b ]


{-| -}
or : Expression Bool -> Expression Bool -> Expression Bool
or (E a) (E b) =
    E <| Any [ a, b ]


{-| -}
eq : Expression a -> Expression a -> Expression Bool
eq (E a) (E b) =
    E <| BinOp Eq a b


{-| -}
neq : Expression a -> Expression a -> Expression Bool
neq (E a) (E b) =
    E <| BinOp NEq a b


{-| -}
gt : Expression a -> Expression a -> Expression Bool
gt (E a) (E b) =
    E <| BinOp Internal.GT a b


{-| -}
lt : Expression a -> Expression a -> Expression Bool
lt (E a) (E b) =
    E <| BinOp Internal.LT a b


{-| -}
lte : Expression a -> Expression a -> Expression Bool
lte (E a) (E b) =
    E <| BinOp LTE a b


{-| -}
gte : Expression a -> Expression a -> Expression Bool
gte (E a) (E b) =
    E <| BinOp GTE a b


{-| -}
all : List (Expression Bool) -> Expression Bool
all bs =
    E <| All <| List.map Internal.unE bs


{-| -}
any : List (Expression Bool) -> Expression Bool
any bs =
    E <| Any <| List.map Internal.unE bs



-- String ----------------------------------------------------------------------


{-| -}
concat : List (Expression String) -> Expression String
concat bs =
    E <| Concat <| List.map Internal.unE bs


{-| -}
downcase : Expression String -> Expression String
downcase (E expr) =
    E <| Downcase expr


{-| -}
upcase : Expression String -> Expression String
upcase (E expr) =
    E <| Upcase expr



-- Color -----------------------------------------------------------------------


{-| -}
rgb : Expression Float -> Expression Float -> Expression Float -> Expression Color
rgb (E r) (E g) (E b) =
    E <| Rgb r g b


{-| -}
rgba : Expression Float -> Expression Float -> Expression Float -> Expression Float -> Expression Color
rgba (E r) (E g) (E b) (E a) =
    E <| Rgba r g b a


{-| -}
toRgba : Expression Color -> Expression (List Float)
toRgba (E color) =
    E <| ToRgba color
