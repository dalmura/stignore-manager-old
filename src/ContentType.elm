module ContentType exposing (ContentType, decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type ContentType
    = ContentType String



-- CREATE


decoder : Decoder ContentType
decoder =
    Decode.map ContentType Decode.string



-- TRANSFORM


encode : ContentType -> Value
encode (ContentType ctype) =
    Encode.string ctype


toString : ContentType -> String
toString (ContentType ctype) =
    ctype
