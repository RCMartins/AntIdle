package pt.rcmartins.antidle.game.saves

import zio.json._

case class UpgradeDataForSave(
    name: String,
    show: Boolean = false,
    unlocked: Boolean = false,
)

object UpgradeDataForSave {

  implicit val decoder: JsonDecoder[UpgradeDataForSave] = DeriveJsonDecoder.gen[UpgradeDataForSave]
  implicit val encoder: JsonEncoder[UpgradeDataForSave] = DeriveJsonEncoder.gen[UpgradeDataForSave]

}
