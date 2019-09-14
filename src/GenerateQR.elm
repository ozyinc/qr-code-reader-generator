module GenerateQR exposing (emptyModel, view, update, Model, Msg)
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import ColorPicker
import Color
import Color.Convert
import Color.Accessibility
import Url
import Url.Builder as Builder
import QRFormat exposing (..)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Tab as Tab

type InvalidError = MinDataLength | MinSize | MaxSize QRFormat Int | MaxDataLength
                  | MarginLow | MarginHigh | QZoneLow | QZoneHigh | LowContrast Float
getCreateError: InvalidError -> String
getCreateError err = case err of
    MinDataLength -> "You must provide data to encode"
    MinSize -> "QR code is too small (needs to be at least 10)"
    MaxSize format maxSize -> "For format " ++ formatToPathString format ++ " maximum size is " ++ String.fromInt maxSize
    MaxDataLength -> "The data can be at most 900 chars of length"
    MarginLow -> "Please provide a positive value for margin"
    MarginHigh -> "Margin can not be above 50"
    QZoneLow -> "Please provide a positive value for qZone"
    QZoneHigh -> "QZone can not be above 100"
    LowContrast currentVal -> "Contrast ratio of the current given colors is " ++ String.fromFloat currentVal ++ ". Please make sure it is at least 3.0 for better looking"

type ErrorCorrection = L | M | Q | H
fromStringToCorrection: String -> ErrorCorrection
fromStringToCorrection str = case str of
    "L" -> L
    "M" -> M
    "Q" -> Q
    "H" -> H
    _ -> L
correctionToPathString: ErrorCorrection -> String
correctionToPathString c = case c of
    L -> "L"
    M -> "M"
    Q -> "Q"
    H -> "H"
correctionToText: ErrorCorrection -> String
correctionToText c = case c of
    L -> "Low"
    M -> "Middle"
    Q -> "Quality"
    H -> "High"



emptyQr: CreatedQRCode
emptyQr = CreatedQRCode "Input Data" 200 False L Color.black Color.white 0 0 SVG
type alias CreatedQRCode = {
      data: String
    , size: Int
    , isoCharset: Bool
    , ecc: ErrorCorrection
    , color: Color.Color
    , bgColor: Color.Color
    , margin : Int
    , qZone: Int
    , format: QRFormat
  }
emptyModel: Model
emptyModel = Model emptyQr ColorPicker.empty ColorPicker.empty Tab.initialState
type alias Model = {
    qrCode: CreatedQRCode
    , colorPicker: ColorPicker.State
    , bgColorPicker: ColorPicker.State
    , tabState: Tab.State
  }
parseInt: String -> Int
parseInt val = Maybe.withDefault 0 (String.toInt val)
type Msg = WriteData String | ChangeSize String | SetCharset Bool
                       | SetErrorCorrection String | ColorPicker ColorPicker.Msg
                       | BgColorPicker ColorPicker.Msg | ChangeMargin String | ChangeQZone String
                       | SetFormat String | TabMsg Tab.State
update: Msg -> Model -> Model
update msg model = let qr = model.qrCode in case msg of
    ColorPicker cMsg -> let (m ,color) = ColorPicker.update cMsg model.qrCode.color model.colorPicker
                        in {model | qrCode = {qr| color = Maybe.withDefault qr.color color}, colorPicker= m}
    BgColorPicker cMsg ->let (m ,color) = ColorPicker.update cMsg model.qrCode.bgColor model.bgColorPicker
                        in {model | qrCode = {qr| bgColor = Maybe.withDefault qr.bgColor color}, bgColorPicker= m}
    TabMsg tState -> {model | tabState = tState}
    other -> let newQr = case other of
                        WriteData data -> {qr | data= data}
                        ChangeSize size -> {qr | size = parseInt size}
                        SetCharset val -> {qr | isoCharset = val}
                        SetErrorCorrection ecc -> {qr | ecc = fromStringToCorrection ecc}
                        ChangeMargin margin -> {qr | margin = parseInt margin}
                        ChangeQZone qZone -> {qr | qZone= parseInt qZone}
                        SetFormat format -> {qr| format = pathToFormatString format}
                        _ -> qr -- The cases are handled in above case statements
             in {model | qrCode = newQr}
getQRUrl: CreatedQRCode -> Result InvalidError String
getQRUrl qr = let contrast = Color.Accessibility.contrastRatio qr.color qr.bgColor in
                if String.length qr.data < 1 then Err MinDataLength
                else if String.length qr.data > 900 then Err MaxDataLength
                else if qr.size < 10 then Err MinSize
                else if qr.size > 1000 && List.member qr.format [PNG, GIF, JPG] then Err (MaxSize qr.format 1000)
                else if qr.size > 1000000 then Err (MaxSize qr.format 1000000)
                else if contrast < 3.0 then Err (LowContrast contrast) -- https://www.w3.org/TR/WCAG20-TECHS/G17.html
                else if qr.margin < 0 then Err MarginLow
                else if qr.margin > 50 then Err MarginHigh
                else if qr.qZone < 0 then Err QZoneLow
                else if qr.qZone > 100 then Err QZoneHigh
                else Ok (buildQRCodeUrl qr)

convertColor: Color.Color -> String
convertColor color = String.dropLeft 1 (Color.Convert.colorToHex color)

buildQRCodeUrl: CreatedQRCode -> String
buildQRCodeUrl qr = Builder.crossOrigin "https://api.qrserver.com/" ["v1", "create-qr-code"] [
  Builder.string "data" qr.data
  , Builder.string "size" (String.fromInt qr.size ++ "x" ++ String.fromInt qr.size)
  , Builder.string "charset-source" (if qr.isoCharset then "ISO-8859-1" else "UTF-8")
  , Builder.string "ecc" (correctionToPathString qr.ecc)
  , Builder.string "color" (convertColor qr.color)
  , Builder.string "bgcolor" (convertColor qr.bgColor)
  , Builder.int "margin" qr.margin
  , Builder.int "qzone" qr.qZone
  , Builder.string "format" (formatToPathString qr.format)
  ]

renderResult: Result InvalidError String -> Html Msg
renderResult res = case res of
    Ok url -> img [A.src url] []
    Err err -> div [] [text ("Error: " ++ getCreateError err)]

viewBasicsForm: CreatedQRCode -> Html Msg
viewBasicsForm qr = Form.form [] [
                        Form.group [] [
                            Form.label [A.for "value"] [ text "Input data"]
                          , Input.text [Input.id "value", Input.onInput WriteData, Input.value qr.data]
                          , Form.help [] [text "Text to be encoded in the QR code"]
                        ]
                      , Form.group [] [
                            Form.label [A.for "size"][ text "Size"]
                          , Input.number [Input.id "size", Input.onInput ChangeSize, Input.value (String.fromInt qr.size)]
                          , Form.help [] [text "Size of the qr code to be processed"]
                        ]
                      , Form.group [] [
                            Form.label [A.for "quality"][ text "Quality"]
                          , Select.select [Select.id "quality", Select.onChange SetErrorCorrection] (List.map (\val -> Select.item [A.id ( correctionToPathString val)] [text (correctionToText val)]) [L,M,Q,H])
                          , Form.help [] [text "Quality of the resulting QR code, better quality is more error-prone"]
                        ]
                      , Form.group [] [
                            Form.label [A.for "format"][ text "Output format"]
                          , Select.select [Select.id "format", Select.onChange SetFormat] (List.map (\val -> let pathString = formatToPathString val in Select.item [A.id pathString, A.value pathString] [text (formatToText val)]) [PNG,GIF,JPG,SVG])
                          , Form.help [] [text "Output format for the QR code, if you need scaling make sure you select SVG"]
                        ]

                    ]
viewColorForm: Model -> Html Msg
viewColorForm state = let qr = state.qrCode in Form.form [] [
                     Form.group [] [
                           Form.label [A.for "color"][ text "FG Color of QR Code"]
                         , div [A.id "color"] [ColorPicker.view qr.color state.colorPicker |> Html.map ColorPicker]
                         -- , Form.help [] [text "Foreground color of QR"]
                       ]
                     , Form.group [] [
                           Form.label [A.for "bgColor"][ text "BG Color of QR Code"]
                         , div [A.id "bgColor"] [ColorPicker.view qr.bgColor state.bgColorPicker |> Html.map BgColorPicker]
                         -- , Form.help [] [text "Background color of QR"]
                       ]
                   ]
viewAdvancedForm: CreatedQRCode -> Html Msg
viewAdvancedForm qr = Form.form [] [
                          Form.group [] [
                              Form.label [A.for "iso"][ text ""]
                            , Checkbox.checkbox [Checkbox.id "iso", Checkbox.onCheck SetCharset, Checkbox.checked qr.isoCharset] "Use ISO-8859-1 charset"
                            , Form.help [] [text "Use the legacy charset to encode the QR code instead of UTF-8"]
                          ]
                        , Form.group [] [
                              Form.label [A.for "margin"][ text "Margin"]
                            , Input.number [Input.id "margin", Input.onInput ChangeMargin, Input.value (String.fromInt qr.margin)]
                            , Form.help [] [text "Margin value for the QR code edges"]
                          ]
                        , Form.group [] [
                              Form.label [A.for "qZone"][ text "QZone"]
                            , Input.number [Input.id "qZone", Input.onInput ChangeQZone, Input.value (String.fromInt qr.qZone)]
                            , Form.help [] [text "QZone value"]
                          ]
                      ]
viewForm: Model -> Html Msg
viewForm state = let qr = state.qrCode in
                   Tab.config TabMsg
                      |> Tab.items [
                        Tab.item {
                           id = "basic",
                           link = Tab.link [] [text "Basics"],
                           pane = Tab.pane [] [viewBasicsForm qr]
                        }
                      , Tab.item {
                          id = "color"
                        , link = Tab.link [] [text "Colors"]
                        , pane = Tab.pane [] [viewColorForm state]
                      }
                      , Tab.item {
                          id = "advanced"
                        , link = Tab.link [] [text "Advanced"]
                        , pane = Tab.pane [] [viewAdvancedForm qr]
                      }
                      ]
                      |> Tab.view state.tabState

viewCard: Model -> Html Msg
viewCard model = Grid.container [] [
        Grid.row [] [
        Grid.col [Col.offsetSm2, Col.sm3] [case getQRUrl model.qrCode of
                                                   Ok url -> img [A.src url, A.style "margin-left" "auto", A.style "margin-right" "auto", A.style "background" "url('loading.gif') no-repeat"] []
                                                   Err err -> Alert.simpleDanger [] [text (getCreateError err)]
                                          ]
        ,Grid.col [ Col.sm5] [
            Card.config [Card.outlinePrimary ]
                |> Card.block [] [Block.custom <| viewForm model]
                |> Card.view
        ] ] ]
view: Model -> Html Msg
view state = viewCard state

