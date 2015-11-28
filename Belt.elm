
module Belt where

import Json.Decode as Json
import String
import Html.Events exposing (targetValue)
import Array exposing (Array)

-- A utility belt module for helper functions


-- Ex: chunkBy 2 [1,2,3,4,5] => [[1,2],[3,4],[5]]
chunkBy' : Int -> List a -> List (List a)
chunkBy' n xs =
  let
    chunk = List.take n xs
  in
    if List.isEmpty chunk then []
    else chunk :: (chunkBy' n (List.drop n xs))

chunkBy : Int -> Array a -> Array (List a)
chunkBy n =
  Array.fromList << (chunkBy' n) << Array.toList

targetValueFloat : Json.Decoder Float
targetValueFloat =
  Json.customDecoder targetValue String.toFloat


targetValueInt : Json.Decoder Int
targetValueInt =
  Json.customDecoder targetValue String.toInt
