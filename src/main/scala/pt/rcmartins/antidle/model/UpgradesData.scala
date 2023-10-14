package pt.rcmartins.antidle.model

import zio.json._

case class UpgradesData(
    queensChamberUpgrade: UpgradeData,
)

object UpgradesData {

  implicit val decoder: JsonDecoder[UpgradesData] = DeriveJsonDecoder.gen[UpgradesData]
  implicit val encoder: JsonEncoder[UpgradesData] = DeriveJsonEncoder.gen[UpgradesData]

  val initial: UpgradesData =
    UpgradesData(
      queensChamberUpgrade = UpgradeData(cost = ActionCost(sugar = 100, colonyPoints = 50)),
    )

}
