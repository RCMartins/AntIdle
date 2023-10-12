package pt.rcmartins.antidle.model

import zio.json._

case class Unlocks(
    canLayEggs: Boolean = false,
    layedFirstEgg: Boolean = false,
    bornFirstWorker: Boolean = false,
    antTasksUnlocked: Boolean = false,
    buildQueueUnlocked: Boolean = false,
    canBuildNest: Boolean = false,
    showColonyPointsResource: Boolean = false,
    larvaeUnlocked: Boolean = false,
    pupaeUnlocked: Boolean = false,
)

object Unlocks {

  implicit val decoder: JsonDecoder[Unlocks] = DeriveJsonDecoder.gen[Unlocks]
  implicit val encoder: JsonEncoder[Unlocks] = DeriveJsonEncoder.gen[Unlocks]

  val initial: Unlocks = Unlocks()

}
