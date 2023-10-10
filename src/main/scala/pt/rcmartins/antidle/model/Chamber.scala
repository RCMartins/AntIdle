package pt.rcmartins.antidle.model

import zio.json._

sealed trait Chamber

object Chamber {

  case class QueenChamber(
      level: Int,
  ) extends Chamber

  object QueenChamber {
    implicit val decoder: JsonDecoder[QueenChamber] = DeriveJsonDecoder.gen[QueenChamber]
    implicit val encoder: JsonEncoder[QueenChamber] = DeriveJsonEncoder.gen[QueenChamber]
  }

  case class NurseryChamber(
      level: Int,
  ) extends Chamber

  object NurseryChamber {
    implicit val decoder: JsonDecoder[NurseryChamber] = DeriveJsonDecoder.gen[NurseryChamber]
    implicit val encoder: JsonEncoder[NurseryChamber] = DeriveJsonEncoder.gen[NurseryChamber]
  }

  case class FoodStorageChamber(
      level: Int,
  ) extends Chamber

  object FoodStorageChamber {
    implicit val decoder: JsonDecoder[FoodStorageChamber] =
      DeriveJsonDecoder.gen[FoodStorageChamber]

    implicit val encoder: JsonEncoder[FoodStorageChamber] =
      DeriveJsonEncoder.gen[FoodStorageChamber]

  }

}
