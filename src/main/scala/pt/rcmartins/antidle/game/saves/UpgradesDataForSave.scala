package pt.rcmartins.antidle.game.saves

import zio.json._

case class UpgradesDataForSave(
    upgrades: Seq[UpgradeDataForSave],
)

object UpgradesDataForSave {

  implicit val decoder: JsonDecoder[UpgradesDataForSave] =
    DeriveJsonDecoder.gen[UpgradesDataForSave]

  implicit val encoder: JsonEncoder[UpgradesDataForSave] =
    DeriveJsonEncoder.gen[UpgradesDataForSave]

}
