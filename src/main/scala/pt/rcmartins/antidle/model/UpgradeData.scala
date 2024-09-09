package pt.rcmartins.antidle.model

import zio.json._

case class UpgradeData(
    show: Boolean = false,
    unlocked: Boolean = false,
)

object UpgradeData {

  implicit val decoder: JsonDecoder[UpgradeData] = DeriveJsonDecoder.gen[UpgradeData]
  implicit val encoder: JsonEncoder[UpgradeData] = DeriveJsonEncoder.gen[UpgradeData]

  val empty: UpgradeData = UpgradeData()

}
