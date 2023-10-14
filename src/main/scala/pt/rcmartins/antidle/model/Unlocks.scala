package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.model.Unlocks._
import zio.json._

case class Unlocks(
    actions: ActionUnlocks,
    tabs: TabUnlocks,
    resources: ResourceUnlocks,
    general: GeneralUnlocks,
)

object Unlocks {

  implicit val decoder: JsonDecoder[Unlocks] = DeriveJsonDecoder.gen[Unlocks]
  implicit val encoder: JsonEncoder[Unlocks] = DeriveJsonEncoder.gen[Unlocks]

  val initial: Unlocks =
    Unlocks(
      actions = ActionUnlocks(),
      tabs = TabUnlocks(),
      resources = ResourceUnlocks(),
      general = GeneralUnlocks(),
    )

  case class ActionUnlocks(
      canLayEggs: Boolean = false,
      canBuildNestUpgrade: Boolean = false,
      canBuildQueenChamber: Boolean = false,
  )

  object ActionUnlocks {
    implicit val decoder: JsonDecoder[ActionUnlocks] = DeriveJsonDecoder.gen[ActionUnlocks]
    implicit val encoder: JsonEncoder[ActionUnlocks] = DeriveJsonEncoder.gen[ActionUnlocks]
  }

  case class TabUnlocks(
      antTasksUnlocked: Boolean = false,
      buildQueueUnlocked: Boolean = false,
      upgradesTabUnlocked: Boolean = false,
  )

  object TabUnlocks {
    implicit val decoder: JsonDecoder[TabUnlocks] = DeriveJsonDecoder.gen[TabUnlocks]
    implicit val encoder: JsonEncoder[TabUnlocks] = DeriveJsonEncoder.gen[TabUnlocks]
  }

  case class ResourceUnlocks(
      showEggs: Boolean = false,
      showColonyPoints: Boolean = false,
      showWorkers: Boolean = false,
  )

  object ResourceUnlocks {
    implicit val decoder: JsonDecoder[ResourceUnlocks] = DeriveJsonDecoder.gen[ResourceUnlocks]
    implicit val encoder: JsonEncoder[ResourceUnlocks] = DeriveJsonEncoder.gen[ResourceUnlocks]
  }

  case class GeneralUnlocks(
//    larvaeUnlocked: Boolean = false,
//    pupaeUnlocked: Boolean = false,
  )

  object GeneralUnlocks {
    implicit val decoder: JsonDecoder[GeneralUnlocks] = DeriveJsonDecoder.gen[GeneralUnlocks]
    implicit val encoder: JsonEncoder[GeneralUnlocks] = DeriveJsonEncoder.gen[GeneralUnlocks]
  }

}
