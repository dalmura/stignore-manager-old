module STIActions exposing (STIActionClass(..), STIActionType(..), STIActions, STIAction, encoder, decoder)

import Agents exposing (Agent)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as Encode exposing (Value)



-- TYPES


type STIActionClass
    = Add
    | Remove


type STIActionType
    = Ignore
    | Keep


type alias STIAction =
    { id : Int
    , agent : Agent
    , actionClass : STIActionClass
    , actionType : STIActionType
    , name : String
    }

type alias STIActions = List STIAction



-- SERIALIZATION

stiActionClassDecoder : Decoder STIActionClass
stiActionClassDecoder =
    Decode.string |>
        Decode.andThen
            (\str ->
              case str of
                "add" -> Decode.succeed Add
                "remove" -> Decode.succeed Remove
                _ -> Decode.fail "Invalid STIActionClass"
            )


stiActionTypeDecoder : Decoder STIActionType
stiActionTypeDecoder =
    Decode.string |>
        Decode.andThen
            (\str ->
              case str of
                "ignore" -> Decode.succeed Ignore
                "keep" -> Decode.succeed Keep
                _ -> Decode.fail "Invalid STIActionType"
            )


stiActionDecoder : Decoder STIAction
stiActionDecoder =
    Decode.succeed STIAction
        |> hardcoded 0
        |> hardcoded (Agents.new "FakeName" "FakeURL")
        |> required "action" stiActionClassDecoder
        |> required "type" stiActionTypeDecoder
        |> required "name" Decode.string

decoder : Decoder STIActions
decoder =
    Decode.list stiActionDecoder

stiActionClassEncoder : STIActionClass -> String
stiActionClassEncoder actionClass =
    case actionClass of
        Add ->
            "add"
        Remove ->
            "remove"

stiActionTypeEncoder : STIActionType -> String
stiActionTypeEncoder actionType =
    case actionType of
        Ignore ->
            "ignore"
        Keep ->
            "keep"

stiActionEncoder : STIAction -> Value
stiActionEncoder action =
    Encode.object
        [ ( "action", Encode.string (stiActionClassEncoder action.actionClass) )
        , ( "type",  Encode.string (stiActionTypeEncoder action.actionType) )
        , ( "name", Encode.string action.name )
        ]

encoder : STIActions -> Value
encoder actions =
    Encode.list stiActionEncoder actions
