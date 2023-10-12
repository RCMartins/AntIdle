package pt.rcmartins.antidle.model

import zio.json._

sealed trait BuildTask {

  def useBuildPower(buildPower: Long): BuildTask

  def isFinished: Boolean

}

object BuildTask {

  implicit val decoder: JsonDecoder[BuildTask] = DeriveJsonDecoder.gen[BuildTask]
  implicit val encoder: JsonEncoder[BuildTask] = DeriveJsonEncoder.gen[BuildTask]

  case class NestUpgrade(buildPowerRequired: Long) extends BuildTask {

    override def useBuildPower(buildPower: Long): BuildTask =
      copy(buildPowerRequired = Math.max(0L, buildPowerRequired - buildPower))

    override def isFinished: Boolean =
      buildPowerRequired == 0

  }

}
