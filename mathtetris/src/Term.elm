module Term exposing (..)

import Browser
import Array exposing (..)
import Html exposing (Html, span, text)
import Html.Attributes exposing (..)

type Term
  = Operand Int
  | Operator Int

operators = Array.fromList ["+", "-", "*", "/", "%"]

init : Int -> Int -> Term
init ty val =
  case ty of
    0 ->
      Operator val
    _ ->
      Operand val

view : Term -> Html a
view term =
  span [ class "term" ] [ text (toText term) ]

toText : Term -> String
toText term =
  case term of
    Operand operand -> String.fromInt operand
    Operator operator ->
      case Array.get (remainderBy (Array.length operators) operator) operators of
        Just text -> text
        Nothing -> " "

type Expr
  = Value Int
  | Partial Term Int
  | Apply Term Int Int
  | Invalid

eval : Array (Maybe Term) -> Maybe Int
eval terms =
  let
    accu =
      \term res ->
        case term of
          Nothing -> Invalid
          Just (Operand val) ->
            case res of
              Value val2 -> Value (val2 * 10 + val)
              Partial (Operator oper) last ->
                Apply (Operator oper) last val
              Apply (Operator oper) last val1 ->
                Apply (Operator oper) last (val1 * 10 + val)
              _ -> Invalid
          Just (Operator oper) ->
            case res of
              Value val2 -> Partial (Operator oper) val2
              Apply (Operator oper1) val1 val2 ->
                case remainderBy (Array.length operators) oper1 of
                  0 ->
                    Partial (Operator oper) (val1 + val2)
                  1 ->
                    Partial (Operator oper) (val1 - val2)
                  2 ->
                    Partial (Operator oper) (val1 * val2)
                  3 ->
                    Partial (Operator oper) (val1 // val2)
                  4 ->
                    Partial (Operator oper) (remainderBy val2 val1)
                  _ -> Invalid
              _ -> Invalid
    expr = Array.foldl accu (Value 0) terms
  in
    case expr of
      Value val ->
        Just val
      Apply (Operator oper) val1 val2 ->
        case remainderBy (Array.length operators) oper of
          0 ->
            Just (val1 + val2)
          1 ->
            Just (val1 - val2)
          2 ->
            Just (val1 * val2)
          3 ->
            Just (val1 // val2)
          4 ->
            Just (remainderBy val2 val1)
          _ -> Nothing
      _ -> Nothing
