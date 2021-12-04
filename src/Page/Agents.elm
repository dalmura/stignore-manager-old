module Page.Agents exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The Agents page.
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


-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }


{-| Recording validation problems on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFieldErrors : ValidatedField -> List Problem -> Html msg

...and it filters the list of problems to render only InvalidEntry ones for the
given ValidatedField. That way you can call this:

viewFieldErrors Email problems

...next to the `email` field, and call `viewFieldErrors Password problems`
next to the `password` field, and so on.

The `LoginError` should be displayed elsewhere, since it doesn't correspond to
a particular field.

-}
type Problem
    = InvalidEntry ValidatedField String


type alias Form =
    { name : String
    , host : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        noAgentsInSession = List.isEmpty (Session.agents session)

        maybeLoadAgents =
            if noAgentsInSession then
                Api.loadAgents (Session.cred session)
                    |> Http.send LoadedAgents
            else
                Cmd.none
    in
    ( { session = session
      , problems = []
      , form =
            { name = ""
            , host = ""
            }
      }
    , maybeLoadAgents
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Agents"
    , content =
        div [ class "agents-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Agents" ]
                        , ul [ class "agent-listing" ]
                            (viewAgents (Session.agents model.session))
                        ]
                    , div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h2 [ class "text-xs-center" ] [ text "Add Agent" ]
                        , ul [ class "error-messages" ]
                            (List.map viewProblem model.problems)
                        , viewForm model.form
                        ]
                    ]
                    , button [ class "btn btn-lg btn-primary pull-xs-right", onClick LoadAgents ]
                        [ text "Load Server Agents" ]
                ]
            ]
    }


viewAgents : Agents.Agents -> List (Html Msg)
viewAgents agents =
    case agents of
        [] ->
            [ li [] [ text "Nothing" ] ]
        items -> 
            List.map viewAgent agents

viewAgent : Agents.Agent -> Html Msg
viewAgent agent =
    li []
      [ text (Agents.pretty agent)
      , text " "
      , button [ onClick (RemoveAgent agent) ] [ text "-" ]
      ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str
    in
    li [] [ text errorMessage ]


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit AddAgent ]
        [ fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "Name"
                , onInput EnteredName
                , value form.name
                ]
                []
            ]
        , fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "Host"
                , onInput EnteredHost
                , value form.host
                ]
                []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Add Agent" ]
        ]



-- UPDATE


type Msg
    = AddAgent
    | RemoveAgent Agent
    | EnteredName String
    | EnteredHost String
    | LoadAgents
    | LoadedAgents (Result Http.Error (List Agent))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAgent ->
            case validate model.form of
                Ok (Trimmed form) ->
                    let
                        agent = Agents.new form.name form.host
                    in
                    ( { model | problems = []
                              , session = Session.addAgent agent model.session
                              , form={name = "", host = ""}
                      }
                    , Cmd.none
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        RemoveAgent agent ->
            ( { model | session = (Session.removeAgent model.session agent) }
            , Cmd.none
            )

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredHost host ->
            updateForm (\form -> { form | host = host }) model

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        LoadAgents ->
            ( model
            , Api.loadAgents (Session.cred model.session)
                |> Http.send LoadedAgents
            )

        LoadedAgents (Ok agents) ->
            ( { model | session = (List.foldl Session.addAgent model.session agents) }
            , Cmd.none
            )

        LoadedAgents (Err error) ->
            ( model, Cmd.none )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Name
    | Host


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Name
    , Host
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Name ->
                if String.isEmpty form.name then
                    [ "name can't be blank." ]

                else
                    []

            Host ->
                if String.isEmpty form.host then
                    [ "host can't be blank." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { name = String.trim form.name
        , host = String.trim form.host
        }



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
