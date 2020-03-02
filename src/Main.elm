port module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Set.Any as Set exposing (AnySet)
import Math.Vector2 as Vector2 exposing (Vec2)
import Element as E
import Element.Input as Input
import Element.Events as Events
import Element.Border as Border
import Element.Background as Background
import Json.Decode as Decode
import List.Extra


port playNote : String -> Cmd msg
port endNote : () -> Cmd msg

type alias Model =
  { startingOctave : Int
  , octaves : Int
  , notesPlaying : AnySet (Octave, Int, Int) Note
  , naturalKeyWidth : Int
  , naturalKeyHeight : Int
  , accidentalKeyWidth : Int
  , accidentalKeyHeight : Int
  , keyMap : List (List Char)
  }


type Note
  = NaturalNote NoteName Octave
  | SharpNote NoteName Octave


type NoteName
  = CNote
  | DNote
  | ENote
  | FNote
  | GNote
  | ANote
  | BNote


comparableNote : Note -> (Octave, Int, Int)
comparableNote note =
  case note of
    NaturalNote name octave ->
      (octave, noteNameToIndex name, 0)
    SharpNote name octave ->
      (octave, noteNameToIndex name, 1)


type alias Octave =
  Int

type Msg
  = PlayNote Note
  | EndNote Note
  | NoOp

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init _ =
  ({ startingOctave = 2
  , octaves = 4
  , notesPlaying = Set.empty comparableNote
  , naturalKeyWidth = 30
  , naturalKeyHeight = 150
  , accidentalKeyWidth = 20
  , accidentalKeyHeight = 90
  , keyMap =
    [ [ '1', '!', '2', '@', '3', '4', '$', '5', '%', '6', '^', '7' ]
    , [ 'q', 'Q', 'w', 'W', 'e', 'r', 'R', 't', 'T', 'y', 'Y', 'u' ]
    , [ 'a', 'A', 's', 'S', 'd', 'f', 'F', 'g', 'G', 'h', 'H', 'j' ]
    , [ 'z', 'Z', 'x', 'X', 'c', 'v', 'V', 'b', 'B', 'n', 'N', 'm' ]
    ]
  }
  , Cmd.none
  )


view : Model -> Html Msg
view model =
  E.layout
    [ E.width E.fill
    ] <|
    E.row
      [ E.width E.fill
      , E.centerX
      , E.centerY
      ]
      [ viewKeyboard model
      ]


keyDecoder : Octave -> List (List Char) -> Decode.Decoder (Maybe Note)
keyDecoder startingOctave keyMap =
  Decode.map (keyToNote startingOctave keyMap) (Decode.field "key" Decode.string)


keyToNote : Octave -> List (List Char) -> String -> Maybe Note
keyToNote startingOctave keyMap key =
  case String.uncons key of
    Just (char, "") ->
      List.head <|
        List.filterMap
          (\(octave, index) ->
            case index of
              Nothing ->
                Nothing
              Just keyIndex ->
                Just <| getNoteAtIndex (startingOctave + octave) (keyIndex + 1)
          )
          (List.indexedMap
            (\index octaveMap ->
              (index, List.Extra.elemIndex char octaveMap)
            )
            keyMap
          )
    _ ->
      Nothing


viewKeyboard : Model -> E.Element Msg
viewKeyboard model =
  let
    width =
      toFloat model.octaves * toFloat model.naturalKeyWidth * 7
    height =
      toFloat model.naturalKeyHeight
  in
  E.row
    [ E.htmlAttribute <| Html.Attributes.style "position" "relative"
    ] <|
    List.map
      (\octave ->
        let
          octaveWidth =
            7 * model.naturalKeyWidth
          position =
            Vector2.vec2 (toFloat octaveWidth * toFloat (octave - model.startingOctave)) (height / 2 - toFloat model.naturalKeyHeight / 2)
        in
        viewOctave model position octave
      )
      (List.range model.startingOctave (model.startingOctave + model.octaves))

viewOctave : Model -> Vec2 -> Octave -> E.Element Msg
viewOctave model position octave =
  let
    noteViews =
      List.map
        (\noteIndex ->
          let
            note =
              getNoteAtIndex octave noteIndex
            deltaX =
              ( case note of
                NaturalNote _ _ ->
                  0
                SharpNote _ _ ->
                  toFloat -model.accidentalKeyWidth / 2
              )
              + ( List.foldl
                (\currNoteIndex distance ->
                  distance
                    + (case getNoteAtIndex octave currNoteIndex of
                    NaturalNote _ _ ->
                      toFloat model.naturalKeyWidth
                    SharpNote _ _ ->
                      0
                    )
                )
                0
                (List.range 1 (noteIndex - 1))
              )
            notePosition =
              Vector2.add position <| Vector2.vec2 deltaX 0
            isPlaying =
              Set.member note model.notesPlaying
        in
        (viewNote model notePosition isPlaying note, note)
      )
      (List.range 1 12)
      
    sortedNoteViews =
      List.sortBy
        (\(_, note) ->
          case note of
            NaturalNote _ _ ->
              0
            SharpNote _ _ ->
              1
        )
        noteViews
  in
  E.row [] <|
    List.map
      (\noteViewTuple ->
        Tuple.first noteViewTuple
      )
    sortedNoteViews


noteNameToIndex : NoteName -> Int
noteNameToIndex name =
  case name of
    CNote ->
      1
    DNote ->
      2
    ENote ->
      3
    FNote ->
      4
    GNote ->
      5
    ANote ->
      6
    BNote ->
      7


noteNameToString : NoteName -> String
noteNameToString name =
  case name of
    CNote ->
      "C"
    DNote ->
      "D"
    ENote ->
      "E"
    FNote ->
      "F"
    GNote ->
      "G"
    ANote ->
      "A"
    BNote ->
      "B"


noteToPitchOctaveNotation : Note -> String
noteToPitchOctaveNotation note =
  case note of
    NaturalNote name octave ->
      noteNameToString name ++ String.fromInt octave
    SharpNote name octave ->
      noteNameToString name ++ "#" ++ String.fromInt octave


getNoteAtIndex : Octave -> Int -> Note
getNoteAtIndex octave noteIndex =
  case noteIndex of
    1 ->
      NaturalNote CNote octave
    2 ->
      SharpNote CNote octave
    3 ->
      NaturalNote DNote octave
    4 ->
      SharpNote DNote octave
    5 ->
      NaturalNote ENote octave
    6 ->
      NaturalNote FNote octave
    7 ->
      SharpNote FNote octave
    8 ->
      NaturalNote GNote octave
    9 ->
      SharpNote GNote octave
    10 ->
      NaturalNote ANote octave
    11 ->
      SharpNote ANote octave
    12 ->
      NaturalNote BNote octave
    _ ->
      -- should never happen
      NaturalNote CNote octave


viewNote : Model -> Vec2 -> Bool -> Note -> E.Element Msg
viewNote model position isPlaying note =
  let
    fillColor =
      if isPlaying then
        Color.lightBlue
      else
        case note of
          NaturalNote _ _ ->
            Color.white
          SharpNote _ _ ->
            Color.black
    attributes =
      ( case note of
        NaturalNote _ _ ->
          [ Html.Attributes.style "width" <| String.fromInt model.naturalKeyWidth ++ "px"
          , Html.Attributes.style "height" <| String.fromInt model.naturalKeyHeight ++ "px"
          ]
        SharpNote _ _ ->
          [ Html.Attributes.style "width" <| String.fromInt model.accidentalKeyWidth ++ "px"
          , Html.Attributes.style "height" <| String.fromInt model.accidentalKeyHeight ++ "px"
          ]
      )
  in
  E.html <|
    Html.button
    ( attributes
    ++ [ Html.Attributes.style "background" <| Color.toCssString fillColor
    , Html.Events.onMouseDown <| PlayNote note
    , Html.Events.onMouseLeave <| EndNote note
    , Html.Attributes.style "border-width" "2px"
    , Html.Attributes.style "border-color" <| Color.toCssString Color.darkGrey
    , Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "left" <| String.fromFloat <| Vector2.getX position
    , Html.Attributes.style "top" <| String.fromFloat <| Vector2.getY position
    ]
    )
    []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlayNote note ->
        ( { model |
          notesPlaying =
            Set.insert note model.notesPlaying
        }
        , if Set.member note model.notesPlaying then
            Cmd.none
          else
            playNote <| noteToPitchOctaveNotation note
        )
    EndNote note ->
      ( { model |
        notesPlaying =
          Set.remove note model.notesPlaying
      }
      , endNote ()
      )
    NoOp ->
      ( model
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onKeyDown
    (Decode.map
      (\note ->
        case note of
          Nothing ->
            NoOp
          Just n ->
            PlayNote n
      )
      <| keyDecoder model.startingOctave model.keyMap
    )
  , Browser.Events.onKeyUp
    (Decode.map
      (\note ->
        case note of
          Nothing ->
            NoOp
          Just n ->
            EndNote n
      )
      <| keyDecoder model.startingOctave model.keyMap
    )
  ]


toElmUIColor : Color.Color -> E.Color
toElmUIColor color =
    let
        {red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha
