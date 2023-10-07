package pt.rcmartins.antidle.model

import zio.json._

sealed trait Chamber

object Chamber {

  implicit val decoder: JsonDecoder[Chamber] = DeriveJsonDecoder.gen[Chamber]
  implicit val encoder: JsonEncoder[Chamber] = DeriveJsonEncoder.gen[Chamber]

  case class QueenChamber(
      level: Int,
  ) extends Chamber

  case class NusaryChamber(
      level: Int,
  ) extends Chamber

  case class FoodStorageChamber(
      level: Int,
  ) extends Chamber

}
