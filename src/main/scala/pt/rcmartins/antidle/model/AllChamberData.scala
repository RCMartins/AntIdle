package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.model.Chamber._
import zio.json._

case class AllChamberData(
    queenChamber: QueenChamber,
)

object AllChamberData {

  implicit val decoder: JsonDecoder[AllChamberData] = DeriveJsonDecoder.gen[AllChamberData]
  implicit val encoder: JsonEncoder[AllChamberData] = DeriveJsonEncoder.gen[AllChamberData]

  val initial: AllChamberData =
    AllChamberData(
      queenChamber = QueenChamber(1),
    )

}
