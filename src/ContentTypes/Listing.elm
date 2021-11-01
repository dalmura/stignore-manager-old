module ContentTypes.Listing exposing (Listing, Item, new, name, size, toTuple, fromTuple, pretty, decoder, encode, toString)

import ContentTypes exposing (ContentType)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as Encode exposing (Value)



-- TYPES

type alias Listing = List Item

type alias Item =
    { name : String
    , size_megabytes : Int
    }



-- INFO

new : String -> Int -> Item
new newName newSize =
    Item newName newSize

name : Item -> String
name item =
    item.name

size : Item -> Int
size item =
    item.size_megabytes

toTuple : Item -> (String, Int)
toTuple item =
    (item.name, item.size_megabytes)

fromTuple : (String, Int) -> Item
fromTuple (newName, newSize) =
    Item newName newSize

pretty : Item -> String
pretty item =
    item.name ++ " (" ++ String.fromInt item.size_megabytes ++ "MB)"

toString : Listing -> String
toString listing =
    List.map pretty listing
        |> String.join ", "



-- SERIALIZATION


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "name" Decode.string
        |> required "size_megabytes" Decode.int

decoder : Decoder Listing
decoder =
    (Decode.list itemDecoder) 

itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "name", Encode.string item.name )
        , ( "size_megabytes", Encode.int item.size_megabytes )
        ]

encode : Listing -> Value
encode listing =
    Encode.list itemEncoder listing
