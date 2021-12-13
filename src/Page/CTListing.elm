module Page.CTListing exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The Content Types Listing page.
-}

import Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)

import Agents exposing (Agents, Agent)
import ContentTypes exposing (ContentTypes, ContentType)
import ContentTypes.Slug exposing (Slug)
import ContentTypes.Listing as CTListing
import STIgnore.Listing as STIListing
import STIActions exposing (STIActionClass(..), STIActionType(..), STIAction, STIActions, AgentSTIActions)
import FlushItems
import Modal

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


type alias Model =
    { session : Session
    , contentType : ContentType
    , ctListings : Dict String CTListing.KVListing
    , stiListings : Dict String STIListing.KVListing
    , flushListings : Dict String FlushItems.Listing
    , nextStiId : Int
    , agentStiActions : AgentSTIActions
    , applyModalOpen : Bool
    , flushModalOpen : Bool
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

        agentFlushListing : ContentType -> Agent -> Cmd Msg
        agentFlushListing ctype agent =
            Api.getFlush (Session.cred session) agent ctype
                |> Http.send GotAgentFlushListing
    in
    ( { session = session
      , contentType = contentType
      , ctListings = Dict.empty
      , stiListings = Dict.empty
      , flushListings = Dict.empty
      , nextStiId = 1
      , agentStiActions = Dict.empty
      , applyModalOpen = False
      , flushModalOpen = False
      }
    , Cmd.batch (
        (List.map (agentCTListing contentType) agents)
        ++
        (List.map (agentSTIListing contentType) agents)
        ++
        (List.map (agentFlushListing contentType) agents)
      )
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Listing"
    , content =
        div [ class "listing-page" ]
            (
                (
                    if model.applyModalOpen then
                        [(renderApplyModal model)]
                    else
                        []
                ) ++
                (
                    if model.flushModalOpen then
                        [(renderFlushModal model)]
                    else
                        []
                ) ++
                [ div [ class "container page" ]
                    [ div [ class "row" ]
                        [ div [ class "col-md-12" ] <|
                            summaryAndActionsTable model.contentType model.agentStiActions model.flushListings
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
            )
    }


renderApplyModal : Model -> Html Msg
renderApplyModal model =
    let
        agentKeys = Dict.keys model.agentStiActions

        agentStiActionsToTable : AgentSTIActions -> String -> Html msg
        agentStiActionsToTable agentStiActions agentKey =
            let
                maybeAgent = Agents.fromKey agentKey
                maybeStiActions = Dict.get agentKey agentStiActions
            in
                case (maybeAgent, maybeStiActions) of
                    (Just agent, Just stiActions) ->
                        if not (List.isEmpty stiActions) then
                            div []
                                [ h3 [] [ text (Agents.name agent) ]
                                , pre [] [ text (Encode.encode 2 (STIActions.encoder stiActions)) ]
                                ]
                        else
                            div [] []
                    (_, _) -> 
                        div [] []

        modalBody =
            [ div [] [ text "We're going to apply:" ]
            , div [] (List.map (agentStiActionsToTable model.agentStiActions) agentKeys)
            , button [ class "btn btn-sm btn-primary", onClick ApplySTIActions ] [ text "Submit Changes" ]
            ]
    in
    Modal.new "apply-modal" "Apply Actions" modalBody CloseApplyModal

renderFlushModal : Model -> Html Msg
renderFlushModal model =
    let
        agentKeys = Dict.keys model.flushListings

        agentFlushItemsToTable : Dict String FlushItems.Listing -> String -> Html msg
        agentFlushItemsToTable flushListings agentKey =
            let
                maybeAgent = Agents.fromKey agentKey
                maybeFlushListing = Dict.get agentKey flushListings
            in
                case (maybeAgent, maybeFlushListing) of
                    (Just agent, Just flushListing) ->
                        if not (List.isEmpty flushListing) then
                            div []
                                [ h3 [] [ text (Agents.name agent) ]
                                , pre [] [ text (Encode.encode 2 (FlushItems.encoder flushListing)) ]
                                ]
                        else
                            div [] []
                    (_, _) ->
                        div [] []

        modalBody =
            [ div [] [ text "We're going to flush:" ]
            , div [] (List.map (agentFlushItemsToTable model.flushListings) agentKeys)
            , button [ class "btn btn-sm btn-primary", onClick FlushItems ] [ text "Flush Changes" ]
            ]
    in
    Modal.new "flush-modal" "Flush Changes" modalBody CloseFlushModal


stiActionToTableRow : Agent -> STIAction -> Html Msg
stiActionToTableRow agent stiAction =
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
        [ td [] [ text actionClass ]
        , td [] [ text (String.Extra.ellipsis 60 stiAction.name) ]
        , td [] [ text actionType ]
        , td [] [ a [ onClick (RemoveSTIAction agent stiAction.id)
                      , href ""
                      , class "tag-pill tag-default"
                    ] [ text "X" ]
                ]
        ]


stiActionsToTable : String -> STIActions -> Html Msg
stiActionsToTable agentKey stiActions =
    let
        maybeAgent = Agents.fromKey agentKey
    in
        case maybeAgent of
            (Just agent) ->
                if List.isEmpty stiActions then
                    div [] []
                else
                    div []
                        [ h1 [] [ text (Agents.name agent)]
                        , table [ class "stilisting-table" ]
                            (
                                [ thead []
                                    [ th [ style "text-align" "center" ] [ text "Action" ]
                                    , th [ style "text-align" "center" ] [ text "Item" ]
                                    , th [ style "text-align" "center" ] [ text "Type" ]
                                    , th [ style "text-align" "center" ] [ text "Remove" ]
                                    ]
                                ]
                                ++ List.map (stiActionToTableRow agent) stiActions
                            )
                        , br [] []
                        ]
            Nothing ->
                div [] [ text ("Unable to parse agent: " ++ agentKey) ]


summaryAndActionsTable : ContentType -> AgentSTIActions -> Dict String FlushItems.Listing -> List (Html Msg)
summaryAndActionsTable ctype agentStiActions flushListings =
    let
        hasStiActions =
            if Dict.isEmpty agentStiActions then
                False
            else if List.isEmpty (List.concat (Dict.values agentStiActions)) then
                False
            else
                True

        hasFlushItems =
            if Dict.isEmpty flushListings then
                False
            else if List.isEmpty (List.concat (Dict.values flushListings)) then
                False
            else
                True

        actionsTable =
            case (hasStiActions, hasFlushItems) of
                (True, True) ->
                    (
                        [ h5 []
                            [ text "Pending Actions"
                            , text " "
                            , button [ class "btn btn-sm btn-primary", onClick ViewApplyModal ] [ text "Apply Changes" ]
                            , text " "
                            , button [ class "btn btn-sm btn-primary", onClick ViewFlushModal ] [ text "Flush Changes" ]
                            ]
                        ]
                        ++ (Dict.map stiActionsToTable agentStiActions
                            |> Dict.values)
                        ++ [ br [] [] ]
                    )

                (True, False) ->
                    (
                        [ h5 []
                            [ text "Pending Actions"
                            , text " "
                            , button [ class "btn btn-sm btn-primary", onClick ViewApplyModal ] [ text "Apply Changes" ]
                            ]
                        ]
                        ++ (Dict.map stiActionsToTable agentStiActions
                            |> Dict.values)
                        ++ [ br [] [] ]
                    )

                (False, True) ->
                    [ h5 []
                        [ button [ class "btn btn-sm btn-primary", onClick ViewFlushModal ] [ text "Flush Changes" ]
                        ]
                    ]

                (False, False) ->
                    []
    in
        (
            [ h2 [] [ text (String.Extra.toTitleCase (ContentTypes.name ctype)) ]
            , br [] []
            ] ++ actionsTable
        )


agentItemStatusToCell : Dict String STIListing.KVListing -> String -> Float -> (Agent, ItemStatus) -> Html Msg
agentItemStatusToCell stiLookup itemName targetSize (agent, itemStatus) =
    let
        maybeStiListing = Dict.get (Agents.key agent) stiLookup

        existsInStiListing =
            case maybeStiListing of
                Just stiListing ->
                    case (Dict.get itemName stiListing) of
                        Just _ ->
                            True
                        Nothing ->
                            False
                Nothing ->
                    False
    in
    case itemStatus of
        Exists size ->
            if (size == targetSize && not existsInStiListing) then
                td [ class "ctlisting-yes" ]
                    [ a [ onClick (AddSTIAction agent Add Ignore itemName)
                        , href ""
                        , class "tag-pill tag-default"
                        ] [ text "yes" ]
                    ]
            else if (size == targetSize && existsInStiListing) then
                td [ class "ctlisting-ignored" ]
                    [ a [ onClick (AddSTIAction agent Remove Ignore itemName)
                        , href ""
                        , class "tag-pill tag-default"
                        ] [ text "yes (flush will delete)" ]
                    ]
            else if (size /= targetSize && not existsInStiListing) then
                td [ class "ctlisting-yes" ] [ text "yes (size conflict)" ]
            else if (size /= targetSize && existsInStiListing) then
                td [ class "ctlisting-yes" ] [ text "yes (size conflict & flush will delete)" ]
            else
                td [ class "ctlisting-yes" ] [ text "yes (unknown state)" ]

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
        agentKey = Agents.key agent
    in
    case (Dict.get agentKey ctLookup) of
        Just ctListing ->
            case (Dict.get itemName ctListing) of
                Just item ->
                    (agent, Exists (CTListing.size item))
                Nothing ->
                    case (Dict.get agentKey stiLookup) of
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
                ++ List.map (agentItemStatusToCell stiLookup itemName maxSize) agentItemStatusRow
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
    | RemoveSTIAction Agent Int
    | GotAgentCTListing (Result Http.Error (Agent, CTListing.Listing))
    | GotAgentSTIListing (Result Http.Error (Agent, STIListing.Listing))
    | GotAgentFlushListing (Result Http.Error (Agent, FlushItems.Listing))
    | ViewApplyModal
    | ViewFlushModal
    | CloseApplyModal
    | CloseFlushModal
    | ApplySTIActions
    | GotAgentApplyResponse (Result Http.Error (Agent, String))
    | FlushItems
    | GotAgentFlushResponse (Result Http.Error (Agent, String))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSTIAction agent actionClass actionType name ->
            let
                agentKey = Agents.key agent

                maybeStiActions = Dict.get agentKey model.agentStiActions

                actionSame : STIActionClass -> STIActionType -> String -> STIAction -> Bool
                actionSame newActionClass newActionType newName curAction =
                    if newActionClass /= curAction.actionClass then
                        False
                    else if newActionType /= curAction.actionType then
                        False
                    else if newName /= curAction.name then
                        False
                    else
                        True

                (newStiActions, actionExistsAlready) =
                    case maybeStiActions of
                        (Just stiActions) ->
                            case (List.any (actionSame actionClass actionType name) stiActions) of
                                True ->
                                    (stiActions, True)
                                False ->
                                    ((stiActions ++ [STIAction model.nextStiId actionClass actionType name]), False)
                        Nothing ->
                            ([STIAction model.nextStiId actionClass actionType name], False)

                nextStiId =
                    if actionExistsAlready then
                        model.nextStiId
                    else
                        model.nextStiId + 1

                newAgentStiActions = Dict.insert agentKey newStiActions model.agentStiActions
            in
            ( { model | agentStiActions = newAgentStiActions, nextStiId = nextStiId }
            , Cmd.none
            )

        RemoveSTIAction agent id ->
            let
                isNotId : Int -> STIAction -> Bool
                isNotId needle item =
                    if item.id == needle then
                        False
                    else
                        True

                agentKey = Agents.key agent

                maybeStiActions = Dict.get agentKey model.agentStiActions
            in
            case maybeStiActions of
                Just stiActions ->
                    let
                        newStiActions = List.filter (isNotId id) stiActions
                        newAgentStiActions = Dict.insert agentKey newStiActions model.agentStiActions
                    in
                    ( { model | agentStiActions = newAgentStiActions }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        GotAgentCTListing (Ok (agent, listing)) ->
            let
                kvlisting = CTListing.toKV listing

                agentKey = Agents.key agent

                newListings =
                    case (Dict.get agentKey model.ctListings) of
                        Just current ->
                            Dict.insert agentKey (Dict.union kvlisting current) model.ctListings

                        Nothing ->
                            Dict.insert agentKey kvlisting model.ctListings
            in
            ( { model | ctListings = newListings }, Cmd.none )

        GotAgentCTListing (Err error) ->
            ( model, Cmd.none )

        GotAgentSTIListing (Ok (agent, listing)) ->
            let
                agentKey = Agents.key agent
                kvlisting = STIListing.toKV listing

                newListings =
                    Dict.insert agentKey kvlisting model.stiListings
            in
            ( { model | stiListings = newListings }, Cmd.none )

        GotAgentSTIListing (Err error) ->
            ( model, Cmd.none )

        GotAgentFlushListing (Ok (agent, listing)) ->
            let
                agentKey = Agents.key agent

                newListings =
                    Dict.insert agentKey listing model.flushListings
            in
            ( { model | flushListings = newListings }, Cmd.none )

        GotAgentFlushListing (Err error) ->
            ( model, Cmd.none )

        ViewApplyModal ->
            ( { model | applyModalOpen = True }, Cmd.none )

        ViewFlushModal ->
            ( { model | flushModalOpen = True }, Cmd.none )

        CloseApplyModal ->
            ( { model | applyModalOpen = False }, Cmd.none )

        CloseFlushModal ->
            ( { model | flushModalOpen = False }, Cmd.none )

        ApplySTIActions ->
            let
                submitActions : (String, STIActions) -> Maybe (Cmd Msg)
                submitActions (agentKey, stiActions) =
                    case (Agents.fromKey agentKey) of
                        Just agent ->
                            Just (
                                Api.stiActions (Session.cred model.session) agent model.contentType stiActions
                                    |> Http.send GotAgentApplyResponse
                            )
                        Nothing ->
                            Nothing

                hasActions : String -> STIActions -> Bool
                hasActions agentKey stiActions =
                    not (List.isEmpty stiActions)

                actionsToSend = Dict.filter hasActions model.agentStiActions
                    |> Dict.toList
            in
                ( model
                , Cmd.batch (List.filterMap submitActions actionsToSend)
                )

        GotAgentApplyResponse (Ok (agent, response)) ->
            let
                agentKey = Agents.key agent
            in
            case (Dict.get agentKey model.agentStiActions) of
                Just stiActions ->
                    let
                        newAgentStiActions = Dict.insert agentKey [] model.agentStiActions
                    in
                    ( { model | agentStiActions = newAgentStiActions }
                    , Cmd.batch
                        [ Api.ctListing (Session.cred model.session) agent model.contentType
                            |> Http.send GotAgentCTListing
                        , Api.stiListing (Session.cred model.session) agent model.contentType
                            |> Http.send GotAgentSTIListing
                        , Api.getFlush (Session.cred model.session) agent model.contentType
                            |> Http.send GotAgentFlushListing
                        ]
                    )
                Nothing ->
                    ( model, Cmd.none )

        GotAgentApplyResponse (Err error) ->
            ( model, Cmd.none )

        FlushItems ->
            ( model, Cmd.none )

        GotAgentFlushResponse (Ok (agent, response)) ->
            ( model, Cmd.none )

        GotAgentFlushResponse (Err error) ->
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
