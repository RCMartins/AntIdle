package pt.rcmartins.antidle.model

import zio.json._

sealed trait AntTask {

  val uiOrder: Int
  val showButtons: Boolean = true

}

object AntTask {

  implicit val decoder: JsonDecoder[AntTask] = DeriveJsonDecoder.gen[AntTask]
  implicit val encoder: JsonEncoder[AntTask] = DeriveJsonEncoder.gen[AntTask]

  case object SugarCollector extends AntTask {
    val uiOrder: Int = 1
  }

  case object WaterCollector extends AntTask {
    val uiOrder: Int = 2
  }

  case object NestBuilder extends AntTask {
    val uiOrder: Int = 3
  }

  case object Nursery extends AntTask {
    val uiOrder: Int = 4
  }

  case object Explorer extends AntTask {
    val uiOrder: Int = 100
    override val showButtons: Boolean = false
  }

}
