package pt.rcmartins.antidle.model

import zio.json._

case class BasicResources(
    sugar: Long,
    proteins: Long,
    water: Long,
    colonyPoints: Long,
    DNA: Long,
) {

  def spendResources(actionCost: ActionCost): BasicResources =
    copy(
      sugar = sugar - actionCost.sugar,
      proteins = proteins - actionCost.proteins,
      water = water - actionCost.water,
      colonyPoints = colonyPoints - actionCost.colonyPoints,
      DNA = DNA - actionCost.DNA,
    )

  def hasResources(cost: ActionCost): Boolean =
    sugar >= cost.sugar &&
      proteins >= cost.proteins &&
      water >= cost.water &&
      colonyPoints >= cost.colonyPoints &&
      DNA >= cost.DNA

}

object BasicResources {

  implicit val decoder: JsonDecoder[BasicResources] = DeriveJsonDecoder.gen[BasicResources]
  implicit val encoder: JsonEncoder[BasicResources] = DeriveJsonEncoder.gen[BasicResources]

  val initial: BasicResources =
    BasicResources(
      sugar = 0,
      proteins = 0,
      water = 0,
      colonyPoints = 0,
      DNA = 0,
    )

}
