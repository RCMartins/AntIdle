package pt.rcmartins.antidle.model

import zio.json._

case class BasicResources(
    sugar: Long,
    proteins: Long,
    water: Long,
    colonyPoints: Long,
    DNA: Long,
)

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
