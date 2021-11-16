module Page.CTListing exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The Content Types Listing page.
-}

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)

import Agents exposing (Agents, Agent)
import ContentTypes exposing (ContentTypes, ContentType)
import ContentTypes.Slug exposing (Slug)
import ContentTypes.Listing as CTListing
import STIgnore.Listing as STIListing

import Dict exposing (Dict)
import Set exposing (Set)

import Round



-- MODEL


type AgentItemStatus
    = Exists Float
    | IgnoredItem
    | MissingItem
    | MissingAgent


type alias Model =
    { session : Session
    , contentType : ContentType
    , ctListings : Dict String CTListing.KVListing
    , stiListings : Dict String STIListing.KVListing
    }



init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        agents =
            Session.agents session

        contentType =
            ContentTypes.fromSlug slug

        agentCTListing : ContentType -> Agent -> Cmd Msg
        agentCTListing ctype agent =
            Api.ctListing (Session.cred session) agent ctype
                |> Http.send GotAgentCTListing

        agentSTIListing : ContentType -> Agent -> Cmd Msg
        agentSTIListing ctype agent =
            Api.stiListing (Session.cred session) agent ctype
                |> Http.send GotAgentSTIListing
    in
    ( { session = session
      , contentType = contentType
      , ctListings = Dict.empty
      , stiListings = Dict.empty
      }
    , Cmd.batch (
        (List.map (agentCTListing contentType) agents)
        ++
        (List.map (agentSTIListing contentType) agents)
      )
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Listing"
    , content =
        div [ class "listing-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-12" ] <|
                        case (Dict.isEmpty model.ctListings) of
                            True ->
                                [ text "No Agents had this Content Type?" ]
                            False ->
                                agentListingsTable (Session.agents model.session) model.ctListings model.stiListings model.contentType
                    ]
                ]
            ]
    }


agentListingsTable : Agents -> Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> ContentType -> List (Html Msg)
agentListingsTable agents ctListings stiListings ctype =
    let
        saveUnique : String -> CTListing.KVListing -> Set String -> Set String
        saveUnique _ listing accum =
            Dict.values listing
                |> List.map CTListing.name
                |> Set.fromList
                |> Set.union accum

        uniqueNames = Dict.foldl saveUnique Set.empty ctListings
            |> Set.toList
            |> List.sort

        toTableHeader : Agent -> Html Msg
        toTableHeader agent = th [] [ text (Agents.name agent) ]

        agentHasItem : Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> String -> String -> AgentItemStatus
        agentHasItem ctLookup stiLookup itemName agentName =
            case (Dict.get agentName ctLookup) of
                Just ctListing ->
                    case (Dict.get itemName ctListing) of
                        Just item ->
                            Exists (CTListing.size item)
                        Nothing ->
                            case (Dict.get agentName stiLookup) of
                                Just stiListing ->
                                    case (Dict.get itemName stiListing) of
                                        Just item ->
                                            IgnoredItem
                                        Nothing ->
                                            MissingItem
                                Nothing ->
                                    MissingItem
                Nothing ->
                    MissingAgent

        toTableRow : Agents -> Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> String -> Html Msg
        toTableRow rowAgents ctLookup stiLookup itemName =
            let
                agentKeys = List.map Agents.pretty rowAgents

                agentItemRow = List.map (agentHasItem ctLookup stiLookup itemName) agentKeys

                itemSize : AgentItemStatus -> Maybe Float
                itemSize itemStatus =
                    case itemStatus of
                        Exists size -> Just size
                        _ -> Nothing

                rowSizes = List.filterMap itemSize agentItemRow

                replicationCount = List.length rowSizes
                replicationCountMin = 2

                maxSize = Maybe.withDefault 0 (List.maximum rowSizes)
                maxSizeHuman = toHumanSize maxSize

                itemNameCell =
                    if replicationCount < replicationCountMin then
                        td [ class "ctlisting-low-replication" ] [ text itemName ]
                    else
                        td [] [ text itemName ]

                itemStatusToCell : Float -> AgentItemStatus -> Html Msg
                itemStatusToCell targetSize itemStatus =
                    case itemStatus of
                        Exists size ->
                            if size == targetSize then
                                td [ class "ctlisting-yes" ] [ text "yes" ]
                            else
                                td [ class "ctlisting-yes" ] [ text "yes (size conflict)" ]
                        IgnoredItem ->
                            td [ class "ctlisting-ignored" ] [ text "ignored" ]
                        MissingItem ->
                            td [ class "ctlisting-no"] [ text "no" ]
                        MissingAgent ->
                            td [] [ text "N/A" ]
            in
            tr []
            (
                [ itemNameCell
                , td [] [ text maxSizeHuman ]
                ]
                ++ List.map (itemStatusToCell maxSize) agentItemRow
            )
    in
    [ div []
        [ table [ class "ctlisting-table" ]
            (
                [ thead []
                    (
                        [ th [] [ text "Item" ]
                        , th [] [ text "Size" ]
                        ]
                        ++ List.map toTableHeader agents
                    )
                ]
                ++ List.map (toTableRow agents ctListings stiListings) uniqueNames
            )
        ]
    ]


toHumanSize : Float -> String
toHumanSize sizeInMB =
    if sizeInMB > 1000 then
        (Round.round 2 (sizeInMB / 1000)) ++ " GB"
    else
        (Round.round 2 (sizeInMB)) ++ " MB"


-- UPDATE


type Msg
    = GotAgentCTListing (Result Http.Error (Agent, CTListing.Listing))
    | GotAgentSTIListing (Result Http.Error (Agent, STIListing.Listing))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAgentCTListing (Ok (agent, listing)) ->
            let
                kvlisting = CTListing.toKV listing

                agentStr = Agents.pretty agent
                maybeExisting = Dict.get agentStr model.ctListings

                newListings =
                    case maybeExisting of
                        Just current ->
                            Dict.insert agentStr (Dict.union kvlisting current) model.ctListings

                        Nothing ->
                            Dict.insert agentStr kvlisting model.ctListings
            in
            ( { model | ctListings = newListings }, Cmd.none )

        GotAgentCTListing (Err error) ->
            ( model, Cmd.none )

        GotAgentSTIListing (Ok (agent, listing)) ->
            let
                kvlisting = STIListing.toKV listing

                agentStr = Agents.pretty agent
                maybeExisting = Dict.get agentStr model.stiListings

                newListings =
                    case maybeExisting of
                        Just current ->
                            Dict.insert agentStr (Dict.union kvlisting current) model.stiListings

                        Nothing ->
                            Dict.insert agentStr kvlisting model.stiListings
            in
            ( { model | stiListings = newListings }, Cmd.none )

        GotAgentSTIListing (Err error) ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
