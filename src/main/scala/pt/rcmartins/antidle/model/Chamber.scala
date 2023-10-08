package pt.rcmartins.antidle.model

sealed trait Chamber

object Chamber {

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
