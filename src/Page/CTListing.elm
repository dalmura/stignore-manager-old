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
import String.Extra

import Round



-- MODEL


type ItemStatus
    = Exists Float
    | IgnoredItem
    | MissingItem
    | MissingAgent


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


type alias Model =
    { session : Session
    , contentType : ContentType
    , ctListings : Dict String CTListing.KVListing
    , stiListings : Dict String STIListing.KVListing
    , nextStiId : Int
    , stiActions : List STIAction
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
      , nextStiId = 1
      , stiActions = []
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
                        summaryAndActionsTable model.contentType model.stiActions
                    ]
                , div [ class "row" ]
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


stiActionToTableRow : STIAction -> Html Msg
stiActionToTableRow stiAction =
    let
        actionClass =
            case stiAction.actionClass of
                Add -> "Add"
                Remove -> "Remove"

        actionType =
            case stiAction.actionType of
                Ignore -> "Ignore"
                Keep -> "Keep"
    in
        tr []
        [ td [] [ text (Agents.name stiAction.agent) ]
        , td [] [ text actionClass ]
        , td [] [ text stiAction.name ]
        , td [] [ text actionType ]
        , td [] [ a [ onClick (RemoveSTIAction stiAction.id)
                      , href ""
                      , class "tag-pill tag-default"
                    ] [ text "X" ]
                ]
        ]


stiActionsToTable : List STIAction -> Html Msg
stiActionsToTable stiActions =
    table [ class "stilisting-table" ]
        (
            [ thead []
                [ th [ style "text-align" "center" ] [ text "Agent" ]
                , th [ style "text-align" "center" ] [ text "Action" ]
                , th [ style "text-align" "center" ] [ text "Item" ]
                , th [ style "text-align" "center" ] [ text "Type" ]
                , th [ style "text-align" "center" ] [ text "Remove" ]
                ]
            ]
            ++ List.map stiActionToTableRow stiActions
        )


summaryAndActionsTable : ContentType -> List STIAction -> List (Html Msg)
summaryAndActionsTable ctype stiActions =
    let
        actionsTable =
            case (List.isEmpty stiActions) of
                True ->
                    []
                False ->
                    [ h4 [] [ text "Pending Actions" ]
                    , stiActionsToTable stiActions
                    , br [] []
                    ]
    in
        (
            [ h2 [] [ text (String.Extra.toTitleCase (ContentTypes.name ctype)) ]
            , br [] []
            ] ++ actionsTable
        )


agentItemStatusToCell : String -> Float -> (Agent, ItemStatus) -> Html Msg
agentItemStatusToCell itemName targetSize (agent, itemStatus) =
    case itemStatus of
        Exists size ->
            if size == targetSize then
                td [ class "ctlisting-yes" ]
                    [ a [ onClick (AddSTIAction agent Add Ignore itemName)
                        , href ""
                        , class "tag-pill tag-default"
                        ] [ text "yes" ]
                    ]
            else
                td [ class "ctlisting-yes" ] [ text "yes (size conflict)" ]
        IgnoredItem ->
            td [ class "ctlisting-ignored" ]
                [ a [ onClick (AddSTIAction agent Remove Ignore itemName)
                    , href ""
                    , class "tag-pill tag-default"
                    ] [ text "ignored" ]
                ]
        MissingItem ->
            td [ class "ctlisting-no"] [ text "no" ]
        MissingAgent ->
            td [] [ text "N/A" ]


itemStatusSize : (Agent, ItemStatus) -> Maybe Float
itemStatusSize (agent, itemStatus) =
    case itemStatus of
        Exists size -> Just size
        _ -> Nothing


agentHasItem : Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> String -> Agent -> (Agent, ItemStatus)
agentHasItem ctLookup stiLookup itemName agent =
    let
        agentName = Agents.pretty agent
    in
    case (Dict.get agentName ctLookup) of
        Just ctListing ->
            case (Dict.get itemName ctListing) of
                Just item ->
                    (agent, Exists (CTListing.size item))
                Nothing ->
                    case (Dict.get agentName stiLookup) of
                        Just stiListing ->
                            case (Dict.get itemName stiListing) of
                                Just item ->
                                    (agent, IgnoredItem)
                                Nothing ->
                                    (agent, MissingItem)
                        Nothing ->
                            (agent, MissingItem)
        Nothing ->
            (agent, MissingAgent)


toHumanSize : Float -> String
toHumanSize sizeInMB =
    if sizeInMB > 1000 then
        (Round.round 2 (sizeInMB / 1000)) ++ " GB"
    else
        (Round.round 2 (sizeInMB)) ++ " MB"


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

        toTableRow : Agents -> Dict String CTListing.KVListing -> Dict String STIListing.KVListing -> String -> Html Msg
        toTableRow rowAgents ctLookup stiLookup itemName =
            let
                agentItemStatusRow = List.map (agentHasItem ctLookup stiLookup itemName) rowAgents
                rowSizes = List.filterMap itemStatusSize agentItemStatusRow

                replicationCount = List.length rowSizes
                replicationCountMin = 2

                maxSize = Maybe.withDefault 0 (List.maximum rowSizes)
                maxSizeHuman = toHumanSize maxSize

                itemNameEllipsis =
                    String.Extra.ellipsis 60 itemName

                itemNameCell =
                    if replicationCount < replicationCountMin then
                        td [ class "ctlisting-low-replication" ] [ text itemNameEllipsis ]
                    else
                        td [] [ text itemNameEllipsis ]
            in
            tr []
            (
                [ itemNameCell
                , td [] [ text maxSizeHuman ]
                ]
                ++ List.map (agentItemStatusToCell itemName maxSize) agentItemStatusRow
            )
    in
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


-- UPDATE



type Msg
    = AddSTIAction Agent STIActionClass STIActionType String
    | RemoveSTIAction Int
    | GotAgentCTListing (Result Http.Error (Agent, CTListing.Listing))
    | GotAgentSTIListing (Result Http.Error (Agent, STIListing.Listing))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSTIAction agent actionClass actionType name ->
            let
                stiAction = STIAction model.nextStiId agent actionClass actionType name
            in
            ( { model | stiActions = (model.stiActions ++ [stiAction]),
                        nextStiId = model.nextStiId + 1
              }, Cmd.none )

        RemoveSTIAction id ->
            let
                isId : Int -> STIAction -> Bool
                isId needle item =
                    if item.id == needle then
                        False
                    else
                        True

                newActions = List.filter (isId id) model.stiActions
            in
            ( { model | stiActions = newActions }, Cmd.none )

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
