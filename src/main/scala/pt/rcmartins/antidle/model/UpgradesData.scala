package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants.u
import zio.json._

// TODO place cost elsewhere?
case class UpgradesData(
    unlockQueensChamber: UpgradeData,
    improveSugarCollectorTask: UpgradeData,
    unlockFoodStorageChamber: UpgradeData,
    unlockExplorerTask: UpgradeData,
)

object UpgradesData {

  implicit val decoder: JsonDecoder[UpgradesData] = DeriveJsonDecoder.gen[UpgradesData]
  implicit val encoder: JsonEncoder[UpgradesData] = DeriveJsonEncoder.gen[UpgradesData]

  val initial: UpgradesData =
    UpgradesData(
      unlockQueensChamber = upgrade(ActionCost(colonyPoints = 50 * u)),
      improveSugarCollectorTask = upgrade(ActionCost(sugar = 100 * u, colonyPoints = 100 * u)),
      unlockFoodStorageChamber = upgrade(ActionCost(colonyPoints = 500 * u)),
      unlockExplorerTask = upgrade(ActionCost(colonyPoints = 1000 * u)),
    )

  @inline private def upgrade(cost: ActionCost): UpgradeData =
    UpgradeData(cost = cost)

}
