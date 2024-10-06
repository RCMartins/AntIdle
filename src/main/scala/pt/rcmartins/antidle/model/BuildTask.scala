package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.model.Chamber.ChamberType
import zio.json._

case class BuildTask(
    chamberType: ChamberType,
    actionCost: ActionCost,
)

object BuildTask {

  implicit val decoder: JsonDecoder[BuildTask] = DeriveJsonDecoder.gen[BuildTask]
  implicit val encoder: JsonEncoder[BuildTask] = DeriveJsonEncoder.gen[BuildTask]

}
