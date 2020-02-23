port module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import TypedSvg as Svg
import TypedSvg.Core exposing (Svg)
import TypedSvg.Attributes
import TypedSvg.Events
import TypedSvg.Types exposing (Paint(..), Transform(..), px)
import Set.Any as Set exposing (AnySet)
import Math.Vector2 as Vector2 exposing (Vec2)


port playNote : String -> Cmd msg
port endNote : () -> Cmd msg

type alias Model =
  { octaves : Int
  , notesPlaying : AnySet (Octave, Int, Int) Note
  , naturalKeyWidth : Float
  , naturalKeyHeight : Float
  , accidentalKeyWidth : Float
  , accidentalKeyHeight : Float
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
  ({ octaves = 5
  , notesPlaying = Set.empty comparableNote
  , naturalKeyWidth = 30
  , naturalKeyHeight = 150
  , accidentalKeyWidth = 20
  , accidentalKeyHeight = 90
  }
  , Cmd.none
  )


width = 1400
height = 900


view : Model -> Html Msg
view model =
  Svg.svg
    [ TypedSvg.Attributes.width (px width)
    , TypedSvg.Attributes.height (px height)
    , TypedSvg.Attributes.viewBox 0 0 width height
    ] <|
    List.map
      (\octave ->
        let
          octaveWidth =
            7 * model.naturalKeyWidth
          position =
            Vector2.vec2 (octaveWidth * toFloat octave) (height / 2 - model.naturalKeyHeight / 2)
        in
        viewOctave model position octave
      )
      (List.range 1 model.octaves)

viewOctave : Model -> Vec2 -> Octave -> Svg Msg
viewOctave model position octave =
  let
    noteViews =
      List.map
        (\noteIndex ->
          let
            note =
              getNoteAtIndex noteIndex octave
            deltaX =
              ( case note of
                NaturalNote _ _ ->
                  0
                SharpNote _ _ ->
                  -model.accidentalKeyWidth / 2
              )
              + ( List.foldl
                (\currNoteIndex distance ->
                  distance
                    + (case getNoteAtIndex currNoteIndex octave of
                    NaturalNote _ _ ->
                      model.naturalKeyWidth
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
  Svg.g [] <|
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


getNoteAtIndex : Int -> Octave -> Note
getNoteAtIndex noteIndex octave =
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


viewNote : Model -> Vec2 -> Bool -> Note -> Svg Msg
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
          [ TypedSvg.Attributes.width <| px model.naturalKeyWidth
          , TypedSvg.Attributes.height <| px model.naturalKeyHeight
          ]
        SharpNote _ _ ->
          [ TypedSvg.Attributes.width <| px model.accidentalKeyWidth
          , TypedSvg.Attributes.height <| px model.accidentalKeyHeight
          ]
      ) ++
      [ TypedSvg.Attributes.x <| px <| Vector2.getX position
      , TypedSvg.Attributes.y <| px <| Vector2.getY position
      , TypedSvg.Attributes.stroke <| Paint Color.darkGrey
      , TypedSvg.Attributes.strokeWidth <| (px 2)
      , TypedSvg.Attributes.fill <| Paint fillColor
      , TypedSvg.Events.onMouseDown <| PlayNote note
      , TypedSvg.Events.onMouseUp <| EndNote note
      ]
  in
  Svg.rect
    attributes
    []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlayNote note ->
      ( { model |
        notesPlaying =
          Set.insert note model.notesPlaying
      }
      , playNote <| noteToPitchOctaveNotation note
      )
    EndNote note ->
      ( { model |
        notesPlaying =
          Set.remove note model.notesPlaying
      }
      , endNote ()
      )


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
