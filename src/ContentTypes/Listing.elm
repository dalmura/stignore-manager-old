module ContentTypes.Listing exposing (Listing, KVListing, Item, new, name, size, toTuple, fromTuple, pretty, decoder, encode, toString, toKV)

import ContentTypes exposing (ContentType)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)

import Dict exposing (Dict)


-- TYPES


type alias Listing = List Item
type alias KVListing = Dict String Item


type alias Item =
    { name : String
    , size_megabytes : Float
    }



-- INFO


new : String -> Float -> Item
new newName newSize =
    Item newName newSize


name : Item -> String
name item =
    item.name


size : Item -> Float
size item =
    item.size_megabytes


toTuple : Item -> (String, Float)
toTuple item =
    (item.name, item.size_megabytes)


fromTuple : (String, Float) -> Item
fromTuple (newName, newSize) =
    Item newName newSize


pretty : Item -> String
pretty item =
    item.name ++ " (" ++ String.fromFloat item.size_megabytes ++ "MB)"


toString : Listing -> String
toString listing =
    List.map pretty listing
        |> String.join ", "


toKV : Listing -> KVListing
toKV listing =
    let
        toAssoc : Item -> (String, Item)
        toAssoc item =
            (item.name, item)
    in
    List.map toAssoc listing
        |> Dict.fromList


-- SERIALIZATION


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "name" Decode.string
        |> required "size_megabytes" Decode.float

decoder : Decoder Listing
decoder =
    (Decode.list itemDecoder) 

itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "name", Encode.string item.name )
        , ( "size_megabytes", Encode.float item.size_megabytes )
        ]

encode : Listing -> Value
encode listing =
    Encode.list itemEncoder listing
