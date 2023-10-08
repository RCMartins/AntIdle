package pt.rcmartins.antidle.model

sealed trait AntTask

object AntTask {

  case object Nursary extends AntTask

  case object SugarCollector extends AntTask

  case object WaterCollector extends AntTask

}
