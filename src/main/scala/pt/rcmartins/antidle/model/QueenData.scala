package pt.rcmartins.antidle.model

import zio.json._

case class QueenData(
)

object QueenData {

  implicit val decoder: JsonDecoder[QueenData] = DeriveJsonDecoder.gen[QueenData]
  implicit val encoder: JsonEncoder[QueenData] = DeriveJsonEncoder.gen[QueenData]

  val initial: QueenData = QueenData()

}
