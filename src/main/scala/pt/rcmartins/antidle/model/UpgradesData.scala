package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants.u
import zio.json._

case class UpgradesData(
    queensChamber: UpgradeData,
    improveSugarCollectorTask: UpgradeData,
)

object UpgradesData {

  implicit val decoder: JsonDecoder[UpgradesData] = DeriveJsonDecoder.gen[UpgradesData]
  implicit val encoder: JsonEncoder[UpgradesData] = DeriveJsonEncoder.gen[UpgradesData]

  val initial: UpgradesData =
    UpgradesData(
      queensChamber = UpgradeData(cost = ActionCost(colonyPoints = 50 * u)),
      improveSugarCollectorTask =
        UpgradeData(cost = ActionCost(sugar = 100 * u, colonyPoints = 100 * u)),
    )

}
