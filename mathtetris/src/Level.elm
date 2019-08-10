module Level exposing (..)

type Level
  = Easy
  | Normal
  | Hard
  | Professional

toText : Level -> String
toText level =
  case level of
    Easy -> "不咋地，十以内算术还掰手指"
    Normal -> "一般，和小卖部老板打成平手"
    Hard -> "优秀，师从体育老师"
    Professional -> "专家，猜中过双色球三个球"

toRange : Level -> Int
toRange level =
  case level of
    Easy ->
      9
    Normal ->
      9
    Hard ->
      9
    Professional ->
      9

toInterval : Level -> Float
toInterval level =
  case level of
    Easy ->
      3000
    Normal ->
      2000
    Hard ->
      1000
    Professional ->
      500