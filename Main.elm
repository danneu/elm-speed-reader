
import StartApp
import Task exposing (Task)
import Effects exposing (Never)
import Html exposing (Html)

import Machine

app : StartApp.App Machine.Model
app =
  StartApp.start
    { update = Machine.update
    , init = Machine.init
    , view = Machine.view
    , inputs = []
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
