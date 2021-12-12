module Agents exposing (Agent(..), Agents, new, fromKey, name, host, pretty, key, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)

import Array


-- TYPES


type Agent
    = Agent Internals


type alias Internals =
    { name : String
    , host : String
    }


type alias Agents = List Agent


new : String -> String -> Agent
new newName newHost =
    Agent (Internals newName newHost)


fromKey : String -> Maybe Agent
fromKey agentKey =
    let
        parts = String.split "!sep!" agentKey
            |> Array.fromList

        tryName = Array.get 0 parts
        tryHost = Array.get 1 parts
    in
    case (tryName, tryHost) of
        (Just newName, Just newHost) ->
            Just (Agent (Internals newName newHost))
        (_, _) ->
            Nothing


-- INFO


name : Agent -> String
name (Agent info) =
    info.name


host : Agent -> String
host (Agent info) =
    info.host


pretty : Agent -> String
pretty (Agent info) =
    info.name ++ " (" ++ info.host ++ ")"


key : Agent -> String
key (Agent info) =
    info.name ++ "!sep!" ++ info.host



-- SERIALIZATION


decoder : Decoder Agent
decoder =
    Decode.succeed Internals
        |> required "name" Decode.string
        |> required "host" Decode.string
        |> Decode.map Agent


encoder : Agent -> Value
encoder (Agent info) =
    Encode.object
        [ ( "name", Encode.string info.name )
        , ( "host", Encode.string info.host )
        ]
