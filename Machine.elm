
-- chrome was at 11%
module Machine where

import Belt

import Time exposing (Time)
import Effects exposing (Effects)
import Html exposing (..)
import Html.Events exposing (onClick, targetValue, on)
import Html.Attributes as Attrs exposing (..)
import String
import Array exposing (Array)

-- MODEL

type alias Model =
  { interval : Time
  , prevBeat : Maybe Time
  , isRunning : Bool
    -- I use Array instead of List so that I can have fast idx lookup
  , chunks : Array (List String)
  , wpm : Int
  , chunkSize : Int
  , chunkIdx : Int
  , wordIdx : Int
  , text : String
  , candidateText : String
  }

wpmToInterval : Int -> Int -> Time
wpmToInterval wpm chunkSize =
  Time.minute / ((toFloat wpm) / (toFloat chunkSize))

init : (Model, Effects Action)
init =
  let
    wpm = 300
    chunkSize = 3
    interval = wpmToInterval wpm chunkSize
    text = initText
  in
    ( { interval = interval
      , wpm = wpm
      , prevBeat = Nothing
      , isRunning = False
      , text = text
      , chunks = Belt.chunkBy chunkSize ((Array.fromList << String.words) text)
      , chunkSize = chunkSize
      , chunkIdx = 0
      , wordIdx = 0
      , candidateText = text
      }
    , Effects.tick Beat
    )

-- UPDATE

type Action
  = Beat Time
  | SetChunkSize Int
  | SetChunkIdx Int
  | SetWpm Int
  | SetCandidateText String -- What user currently has typed into textarea
  | PromoteCandidateText -- Load the textarea into the speed reader
  | Toggle -- Toggles between running/paused
  | Reset -- Pause and go back to the beginning

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Toggle ->
      ( { model | isRunning = not model.isRunning }
      , Effects.none
      )
    Reset ->
      ( { model | prevBeat = Nothing
                , isRunning = False
                , chunkIdx = 0
                , wordIdx = 0
        }
      , Effects.none
      )
    SetChunkSize newChunkSize ->
      let
        newChunkSize' = clamp 1 10 newChunkSize
        newChunkIdx = model.wordIdx // newChunkSize'
        newInterval = wpmToInterval model.wpm newChunkSize'
        newChunks = Belt.chunkBy newChunkSize' ((Array.fromList << String.words) model.text)
      in
        ( { model | interval = newInterval
                  , chunkSize = newChunkSize'
                  , chunkIdx = newChunkIdx
                  , chunks = newChunks
          }
        , Effects.none
        )
    SetWpm wpm ->
      let
        wpm' = clamp 10 1000 wpm
        newInterval = wpmToInterval wpm' model.chunkSize
      in
        ( { model | interval = newInterval
                  , wpm = wpm'
          }
        , Effects.none
        )
    SetCandidateText text ->
      ( { model | candidateText = text }
      , Effects.none
      )
    PromoteCandidateText ->
      let
        newChunks =
          (Array.fromList << String.words) model.candidateText
          |> Belt.chunkBy model.chunkSize
      in
      ( { model | isRunning = False
                , chunks = newChunks
                , text = model.candidateText
        }
      , Effects.none
      )
    SetChunkIdx newChunkIdx ->
      let
        newChunkIdx' =
          clamp 0 ((Array.length model.chunks) - 1)
          <| newChunkIdx
      in
        ( { model | prevBeat = Nothing
                  , isRunning = False
                  , wordIdx = newChunkIdx' * model.chunkSize
                  , chunkIdx = newChunkIdx'
          }
        , Effects.none
        )
    Beat now ->
      case (model.isRunning, model.prevBeat) of
        -- When paused, always just schedule next Beat
        (False, _) ->
          ( model
          , Effects.tick Beat
          )
        -- Wait a full tick at the very start
        (_, Nothing) ->
          ( { model | prevBeat = Just now }
          , Effects.tick Beat
          )
        (_, Just prev) ->
          let
            newModel =
              if (now - prev >= model.interval) then
                let
                  -- Overflow back to idx 0 when we hit the end
                  newChunkIdx = (model.chunkIdx + 1) % (Array.length model.chunks)
                  newWordIdx = newChunkIdx * model.chunkSize
                in
                  { model | prevBeat = (Just now)
                          , chunkIdx = newChunkIdx
                          , wordIdx = newWordIdx
                  }
              else
                model
          in
            ( newModel
            , Effects.tick Beat
            )

-- VIEW

(=>) : a -> b -> (a, b)
(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ class "container" ]
  [ h1
    [ class "text-center"
    , style ["margin-bottom" => "100px"]
    ]
    [ text <| String.join " "
           <| Maybe.withDefault []
           <| Array.get model.chunkIdx model.chunks
    ]
  , div
    [ class "panel panel-default" ]
    [ div
      [ class "panel-body" ]
      [ -- Progress scrubber
        input
        [ type' "range"
        , Attrs.min "0"
        , Attrs.max (toString (Array.length model.chunks - 1))
        , step "1"
        , value (toString model.chunkIdx)
        , on "input" Belt.targetValueInt (Signal.message address << SetChunkIdx)
        ]
        []
      , hr [] []
        -- Settings
      , div
        [ class "text-center" ]
        [ button
          [ onClick address Toggle
          , classList [ "btn" => True
                      , "btn-default" => model.isRunning
                      , "btn-success" => not model.isRunning
                      ]
          ]
          ( if model.isRunning then
              [ span
                [ class "rotate glyphicon glyphicon-refresh" ]
                []
              , text " Pause"
              ]
            else
              [ text "Play" ]
          )
        , text " "
        , button
          [ onClick address Reset
          , class "btn btn-default"
          ]
          [ text "Reset" ]
        ]
        -- ez config row
      , div
        [ class "text-center"
        , style [ "margin-top" => "10px" ]
        ]
        [ text <| (toString model.wpm) ++ " WPM: "
        , div
          [ class "btn-group btn-group-xs" ]
          [ button
            [ class "btn btn-default"
            , onClick address (SetWpm (model.wpm - 10))
            ]
            [ text "Slower" ]
          , button
            [ class "btn btn-default"
            , onClick address (SetWpm (model.wpm + 10))
            ]
            [ text "Faster" ]
          ]
        , text <| " @ " ++ (toString model.chunkSize) ++ " per chunk: "
        , div
          [ class "btn-group btn-group-xs" ]
          [ button
            [ class "btn btn-default"
            , onClick address (SetChunkSize (model.chunkSize - 1))
            ]
            [ text "Smaller" ]
          , button
            [ class "btn btn-default"
            , onClick address (SetChunkSize (model.chunkSize + 1))
            ]
            [ text "Larger" ]
          ]
        ]
      ]
    ]
  , h3 [] [ text "Text" ]
  , textarea
    [ value model.candidateText
    , class "form-control"
    , on "input" targetValue (Signal.message address << SetCandidateText)
    , rows 6
    ]
    []
  , button
    [ class "btn btn-default"
    , onClick address PromoteCandidateText
    ]
    [ text "Save" ]
  , h3 [] [ text "State" ]
  , p [] [ text (toString { model | chunks = [["<snip>"]], candidateText = "<snip>", text = "<snip>" }) ]
  , footer
    [ style [ "margin-top" => "100px"
            , "text-align" => "center"
            ]
    ]
    [ p
      []
      [ text "Source code at "
      , a
        [ href "https://github.com/danneu/elm-speed-reader" ]
        [ text "danneu/elm-speed-reader" ]
      ]
    ]
  ]

-- put it down here so it's not in the center of the code
initText : String
initText = """
Speed reading is the art of silencing subvocalization. Most readers have an average reading speed of 200 wpm, which is about as fast as they can read a passage out loud. This is no coincidence. It is their inner voice that paces through the text that keeps them from achieving higher reading speeds. They can only read as fast as they can speak because that's the way they were taught to read, through reading systems like Hooked on Phonics.

However, it is entirely possible to read at a much greater speed, with much better reading comprehension, by silencing this inner voice. The solution is simple - absorb reading material faster than that inner voice can keep up.

In the real world, this is achieved through methods like reading passages using a finger to point your way. You read through a page of text by following your finger line by line at a speed faster than you can normally read. This works because the eye is very good at tracking movement. Even if at this point full reading comprehension is lost, it's exactly this method of training that will allow you to read faster.

With the aid of software like Spreeder, it's much easier to achieve this same result with much less effort. Load a passage of text (like this one), and the software will pace through the text at a predefined speed that you can adjust as your reading comprehension increases.

To train to read faster, you must first find your base rate. Your base rate is the speed that you can read a passage of text with full comprehension. We've defaulted to 300 wpm, showing one word at a time, which is about the average that works best for our users. Now, read that passage using spreeder at that base rate.

After you've finished, double that speed by going to the Settings and changing the Words Per Minute value. Reread the passage. You shouldn't expect to understand everything - in fact, more likely than not you'll only catch a couple words here and there. If you have high comprehension, that probably means that you need to set your base rate higher and rerun this test again. You should be straining to keep up with the speed of the words flashing by. This speed should be faster than your inner voice can \"read\".

Now, reread the passage again at your base rate. It should feel a lot slower - if not, try running the speed test again). Now try moving up a little past your base rate - for example, at 400 wpm - , and see how much you can comprehend at that speed.

That's basically it - constantly read passages at a rate faster than you can keep up, and keep pushing the edge of what you're capable of. You'll find that when you drop down to lower speeds, you'll be able to pick up much more than you would have thought possible.

One other setting that's worth mentioning in this introduction is the chunk size - the number of words that are flashed at each interval on the screen. When you read aloud, you can only say one word at a time. However, this limit does not apply to speed reading. Once your inner voice subsides and with constant practice, you can read multiple words at a time. This is the best way to achieve reading speeds of 1000+ wpm. Start small with 2 word chunk sizes and find out that as you increase, 3,4, or even higher chunk sizes are possible.

Good luck!
"""
