package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants.u
import zio.json._

// TODO place cost elsewhere?
case class UpgradesData(
    unlockQueensChamber: UpgradeData,
    improveSugarCollectorTask1: UpgradeData,
    improveSugarCollectorTask2: UpgradeData,
    improveSugarCollectorTask3: UpgradeData,
    unlockFoodStorageChamber: UpgradeData,
    unlockExplorerTask: UpgradeData,
)

object UpgradesData {

  implicit val decoder: JsonDecoder[UpgradesData] = DeriveJsonDecoder.gen[UpgradesData]
  implicit val encoder: JsonEncoder[UpgradesData] = DeriveJsonEncoder.gen[UpgradesData]

  val initial: UpgradesData =
    UpgradesData(
      unlockQueensChamber = upgrade(ActionCost(colonyPoints = 50 * u)),
      improveSugarCollectorTask1 = upgrade(ActionCost(sugar = 100 * u, colonyPoints = 100 * u)),
      improveSugarCollectorTask2 = upgrade(ActionCost(sugar = 500 * u, colonyPoints = 500 * u)),
      improveSugarCollectorTask3 = upgrade(ActionCost(sugar = 2500 * u, colonyPoints = 2500 * u)),
      unlockFoodStorageChamber = upgrade(ActionCost(colonyPoints = 500 * u)),
      unlockExplorerTask = upgrade(ActionCost(colonyPoints = 1000 * u)),
    )

  @inline private def upgrade(cost: ActionCost): UpgradeData =
    UpgradeData(cost = cost)

}
