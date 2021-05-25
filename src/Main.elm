module Main exposing (..)

import Bootstrap.Navbar as Navbar
import Browser
import GenerateQR
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import ReadQR



-- Read Logic
-- Model


type QRTab
    = ReadQR
    | GenerateQR


emptyNavbar =
    Navbar.initialState NavbarMsg


emptyModel : Model
emptyModel =
    Model ReadQR GenerateQR.emptyModel ReadQR.emptyModel (Tuple.first emptyNavbar)


type alias Model =
    { activeTab : QRTab
    , createState : GenerateQR.Model
    , readState : ReadQR.Model
    , navbarState : Navbar.State
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Tuple.second emptyNavbar )



---- UPDATE ----


type Msg
    = CreateMsg GenerateQR.Msg
    | SwitchTab QRTab
    | ReadMsg ReadQR.Msg
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        CreateMsg cMsg ->
            ( { model | createState = GenerateQR.update cMsg model.createState }, Cmd.none )

        ReadMsg rMsg ->
            let
                ( newState, cmd ) =
                    ReadQR.update rMsg model.readState
            in
            ( { model | readState = newState }, Cmd.map ReadMsg cmd )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Navbar.config NavbarMsg
            |> Navbar.brand [] [ text "QR Coder" ]
            |> Navbar.light
            |> Navbar.items
                [ (if model.activeTab == ReadQR then
                    Navbar.itemLinkActive

                   else
                    Navbar.itemLink
                  )
                    [ E.onClick (SwitchTab ReadQR), A.href "#" ]
                    [ text "Read QR" ]
                , (if model.activeTab == GenerateQR then
                    Navbar.itemLinkActive

                   else
                    Navbar.itemLink
                  )
                    [ E.onClick (SwitchTab GenerateQR), A.href "#" ]
                    [ text "Generate QR" ]
                ]
            |> Navbar.view model.navbarState
        , case model.activeTab of
            ReadQR ->
                Html.map ReadMsg (ReadQR.view model.readState)

            GenerateQR ->
                Html.map CreateMsg (GenerateQR.view model.createState)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
