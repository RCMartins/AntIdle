package pt.rcmartins.antidle.model

import zio.json._

sealed trait AntTask

object AntTask {

  implicit val decoder: JsonDecoder[AntTask] = DeriveJsonDecoder.gen[AntTask]
  implicit val encoder: JsonEncoder[AntTask] = DeriveJsonEncoder.gen[AntTask]

  case object Nursary extends AntTask

  case object SugarCollector extends AntTask

  case object WaterCollector extends AntTask

}
