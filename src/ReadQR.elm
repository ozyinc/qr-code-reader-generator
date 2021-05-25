module ReadQR exposing (Model, Msg, emptyModel, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Text
import File
import File.Select as Select
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import QRFormat exposing (..)
import Url


type QRError
    = SlowConnection
    | Offline
    | BadStatus Int String
    | InvalidUrl String
    | FailedOnServer String


qrErrorToString : QRError -> String
qrErrorToString err =
    case err of
        SlowConnection ->
            "Request timed-out, is your internet fast enough?"

        Offline ->
            "You appear to be disconnected. I need internet to work"

        BadStatus status body ->
            "Server has send an invalid HTTP status code: " ++ String.fromInt status ++ " with body: " ++ body

        InvalidUrl url ->
            "The request trying to be send was invalid, namely: " ++ url

        FailedOnServer serverErr ->
            "The server had an error: " ++ serverErr


type alias RawSymbol =
    { seq : Int
    , data : Maybe String
    , error : Maybe String
    }


type alias RawResponse =
    { type_ : String
    , symbol : List RawSymbol
    }


responseDecoder : Decode.Decoder RawSymbol
responseDecoder =
    Decode.list
        (Decode.succeed RawResponse
            |> required "type" Decode.string
            |> required "symbol"
                (Decode.list
                    (Decode.succeed RawSymbol
                        |> required "seq" Decode.int
                        |> required "data" (Decode.maybe Decode.string)
                        |> required "error" (Decode.maybe Decode.string)
                    )
                )
        )
        |> Decode.andThen
            (\rawResponse ->
                case rawResponse of
                    [] ->
                        Decode.fail "Empty Response from server"

                    x :: xs ->
                        case x.symbol of
                            [] ->
                                Decode.fail "Empty response body from server"

                            y :: ys ->
                                Decode.succeed y
            )


expectQR : (ReadResult -> msg) -> Http.Expect msg
expectQR toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (InvalidUrl url)

                Http.Timeout_ ->
                    Err SlowConnection

                Http.NetworkError_ ->
                    Err Offline

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata.statusCode body)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString responseDecoder body of
                        Ok value ->
                            case value.error of
                                Nothing ->
                                    case value.data of
                                        Nothing ->
                                            Err (FailedOnServer "Empty response data without error")

                                        Just data ->
                                            Ok data

                                Just erre ->
                                    Err (FailedOnServer erre)

                        Err errString ->
                            Err (FailedOnServer (Decode.errorToString errString))


localBase : String
localBase =
    "/forward.php"


readUrl : String -> Cmd Msg
readUrl url =
    Http.get { url = localBase ++ "?fileurl=" ++ Url.percentEncode url, expect = expectQR (ReadComplete Url) }


readFile : File.File -> Cmd Msg
readFile file =
    Http.post { url = localBase, body = Http.multipartBody [ Http.filePart "file" file ], expect = expectQR (ReadComplete File) }


type alias ReadResult =
    Result QRError String


type ReadResource
    = Url
    | File


type Msg
    = SetUrl String
    | PerformRead ReadResource
    | ReadComplete ReadResource ReadResult
    | SetFile File.File
    | FileUploadRequested


type FileError
    = MaxSizeExceeded Int


type alias FileResult =
    Result FileError File.File


fileErrorToString : FileError -> String
fileErrorToString err =
    case err of
        MaxSizeExceeded currSize ->
            "Given file size " ++ String.fromInt currSize ++ "B is bigger than maximum file size allowed, which is " ++ String.fromInt maxFileSize ++ "B!"


emptyModel : Model
emptyModel =
    Model "" Nothing Nothing Nothing


type alias Model =
    { url : String
    , urlResult : Maybe ReadResult
    , file : Maybe FileResult
    , fileResult : Maybe ReadResult
    }


maxFileSize : Int
maxFileSize =
    1048576


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUrl url ->
            ( { model | url = url, urlResult = Nothing }, Cmd.none )

        PerformRead resource ->
            let
                cmd =
                    case resource of
                        File ->
                            case model.file of
                                Just result ->
                                    case result of
                                        Ok file ->
                                            readFile file

                                        Err _ ->
                                            Cmd.none

                                Nothing ->
                                    Cmd.none

                        Url ->
                            readUrl model.url
            in
            ( model, cmd )

        ReadComplete resource result ->
            case resource of
                Url ->
                    ( { model | urlResult = Just result }, Cmd.none )

                File ->
                    ( { model | fileResult = Just result }, Cmd.none )

        SetFile file ->
            let
                newFile =
                    if File.size file < maxFileSize then
                        Just (Ok file)

                    else
                        Just (Err (MaxSizeExceeded (File.size file)))
            in
            ( { model | file = newFile, fileResult = Nothing }, Cmd.none )

        FileUploadRequested ->
            ( model, Select.file (List.map formatToMime readList) SetFile )


viewUrlFormRow : Maybe ReadResult -> Html Msg
viewUrlFormRow readResult =
    let
        rRes =
            convertRead readResult
    in
    Fieldset.config
        |> Fieldset.legend [ A.class "text-center" ] [ text "Read from URL" ]
        |> Fieldset.children
            [ Form.row []
                [ Form.col [ Col.offsetSm3, Col.sm5 ] [ Input.url [ Input.onInput SetUrl, Input.placeholder "Url" ] ]
                , Form.col [ Col.sm2 ]
                    [ Button.button
                        [ Button.primary
                        , Button.onClick (PerformRead Url)
                        ]
                        [ text "Read" ]
                    ]
                ]
            , case rRes of
                Error err ->
                    Alert.simpleDanger [] [ text (qrErrorToString err) ]

                Correct val ->
                    Alert.simpleSuccess [] [ text ("Parsing done! Value is: " ++ val) ]

                _ ->
                    text ""
            ]
        |> Fieldset.view


type InputType a err
    = None
    | Error err
    | Correct a


convertRead : Maybe (Result a b) -> InputType b a
convertRead res =
    case res of
        Just r ->
            case r of
                Ok o ->
                    Correct o

                Err err ->
                    Error err

        Nothing ->
            None


viewFileFormRow : Maybe FileResult -> Maybe ReadResult -> Html Msg
viewFileFormRow fileResult readResult =
    let
        ( fRes, rRes ) =
            ( convertRead fileResult, convertRead readResult )
    in
    Fieldset.config
        |> Fieldset.legend [ A.class "text-center" ] [ text "Read from file" ]
        |> Fieldset.children
            [ Form.row []
                [ Form.col [ Col.offsetSm3, Col.sm4 ]
                    [ text <|
                        case fRes of
                            Correct file ->
                                "Selected file: " ++ File.name file

                            _ ->
                                "Select file to upload"
                    ]
                , Form.col [ Col.sm1 ] [ Button.button [ Button.onClick FileUploadRequested, Button.primary ] [ text "Select" ] ]
                , Form.col [ Col.sm2 ]
                    [ Button.button
                        [ Button.primary
                        , case fRes of
                            Correct _ ->
                                Button.onClick (PerformRead File)

                            _ ->
                                Button.disabled True
                        ]
                        [ text "Read" ]
                    ]
                ]
            , case fRes of
                Error err ->
                    Alert.simpleDanger [] [ text (fileErrorToString err) ]

                _ ->
                    text ""
            , case rRes of
                Error err ->
                    Alert.simpleDanger [] [ text (qrErrorToString err) ]

                Correct val ->
                    Alert.simpleSuccess [] [ text ("Parsing done! Value is: " ++ val) ]

                _ ->
                    text ""
            ]
        |> Fieldset.view


view : Model -> Html Msg
view model =
    div []
        [ Form.form []
            [ viewUrlFormRow model.urlResult
            , viewFileFormRow model.file model.fileResult
            ]
        ]
