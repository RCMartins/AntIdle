package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.model.Chamber.ChamberType
import zio.json._

sealed trait Chamber {

  def level: Int

  def chamberType: ChamberType

}

object Chamber {

  sealed trait ChamberType

  object ChamberType {

    case object Nest extends ChamberType

    case object Queen extends ChamberType

    case object FoodStorage extends ChamberType

    case object Nursery extends ChamberType

  }

  case class NestChamber(
      level: Int,
  ) extends Chamber {
    val chamberType: ChamberType = ChamberType.Nest
  }

  object NestChamber {
    implicit val decoder: JsonDecoder[NestChamber] = DeriveJsonDecoder.gen[NestChamber]
    implicit val encoder: JsonEncoder[NestChamber] = DeriveJsonEncoder.gen[NestChamber]
  }

  case class QueenChamber(
      level: Int,
  ) extends Chamber {
    val chamberType: ChamberType = ChamberType.Queen
  }

  object QueenChamber {
    implicit val decoder: JsonDecoder[QueenChamber] = DeriveJsonDecoder.gen[QueenChamber]
    implicit val encoder: JsonEncoder[QueenChamber] = DeriveJsonEncoder.gen[QueenChamber]
  }

  case class FoodStorageChamber(
      level: Int,
  ) extends Chamber {
    val chamberType: ChamberType = ChamberType.FoodStorage
  }

  object FoodStorageChamber {
    implicit val decoder: JsonDecoder[FoodStorageChamber] =
      DeriveJsonDecoder.gen[FoodStorageChamber]

    implicit val encoder: JsonEncoder[FoodStorageChamber] =
      DeriveJsonEncoder.gen[FoodStorageChamber]

  }

  case class NurseryChamber(
      level: Int,
  ) extends Chamber {
    val chamberType: ChamberType = ChamberType.Nursery
  }

  object NurseryChamber {
    implicit val decoder: JsonDecoder[NurseryChamber] = DeriveJsonDecoder.gen[NurseryChamber]
    implicit val encoder: JsonEncoder[NurseryChamber] = DeriveJsonEncoder.gen[NurseryChamber]
  }

}
