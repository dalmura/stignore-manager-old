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
import ContentTypes.Listing as Listing exposing (Listing, KVListing, Item)

import Dict exposing (Dict)
import Set exposing (Set)

import Round



-- MODEL


type AgentItemStatus
    = Exists Float
    | MissingItem
    | MissingAgent


type alias Model =
    { session : Session
    , contentType : ContentType
    , listings : Dict String KVListing
    }



init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        agents =
            Session.agents session

        contentType =
            ContentTypes.fromSlug slug

        agentListing : ContentType -> Agent -> Cmd Msg
        agentListing ctype agent =
            Api.listing (Session.cred session) agent ctype
                |> Http.send GotAgentListing
    in
    ( { session = session
      , contentType = contentType
      , listings = Dict.empty
      }
    , Cmd.batch (List.map (agentListing contentType) agents)
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
                        case (Dict.isEmpty model.listings) of
                            True ->
                                [ text "No Agents had this Content Type?" ]
                            False ->
                                agentListingsTable (Session.agents model.session) model.listings model.contentType
                    ]
                ]
            ]
    }


agentListingsTable : Agents -> Dict String KVListing -> ContentType -> List (Html Msg)
agentListingsTable agents listings ctype =
    let
        saveUnique : String -> KVListing -> Set String -> Set String
        saveUnique _ listing accum =
            Dict.values listing
                |> List.map Listing.name
                |> Set.fromList
                |> Set.union accum

        uniqueNames = Dict.foldl saveUnique Set.empty listings
            |> Set.toList
            |> List.sort

        toTableHeader : Agent -> Html Msg
        toTableHeader agent = th [] [ text (Agents.name agent) ]

        agentHasItem : Dict String KVListing -> String -> String -> AgentItemStatus
        agentHasItem lookup itemName agentName =
            case (Dict.get agentName lookup) of
                Just kvlisting ->
                    case (Dict.get itemName kvlisting) of
                        Just item ->
                            Exists (Listing.size item)
                        Nothing ->
                            MissingItem
                Nothing ->
                    MissingAgent

        toTableRow : Agents -> Dict String KVListing -> String -> Html Msg
        toTableRow rowAgents lookup itemName =
            let
                agentKeys = List.map Agents.pretty rowAgents

                agentItemRow = List.map (agentHasItem lookup itemName) agentKeys

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
                ++ List.map (toTableRow agents listings) uniqueNames
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
    = GotAgentListing (Result Http.Error (Agent, Listing))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAgentListing (Ok (agent, listing)) ->
            let
                kvlisting = Listing.toKV listing

                agentStr = Agents.pretty agent
                maybeExisting = Dict.get agentStr model.listings

                newListings =
                    case maybeExisting of
                        Just current ->
                            Dict.insert agentStr (Dict.union kvlisting current) model.listings

                        Nothing ->
                            Dict.insert agentStr kvlisting model.listings
            in
            ( { model | listings = newListings }, Cmd.none )

        GotAgentListing (Err error) ->
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
