package pt.rcmartins.antidle.model

import zio.json._

sealed trait BuildTask {

  type T <: BuildTask

  def buildPowerRequired: Long

  def newInstance(newBuildPower: Long): T

  def useBuildPower(buildPower: Long): T =
    newInstance(Math.max(0L, buildPowerRequired - buildPower))

  def isFinished: Boolean =
    buildPowerRequired == 0

}

object BuildTask {

  implicit val decoder: JsonDecoder[BuildTask] = DeriveJsonDecoder.gen[BuildTask]
  implicit val encoder: JsonEncoder[BuildTask] = DeriveJsonEncoder.gen[BuildTask]

  case class NestUpgrade(buildPowerRequired: Long) extends BuildTask {

    type T = NestUpgrade

    override def newInstance(newBuildPower: Long): T =
      copy(buildPowerRequired = newBuildPower)

  }

  case class QueenChamber(buildPowerRequired: Long) extends BuildTask {

    type T = QueenChamber

    override def newInstance(newBuildPower: Long): T =
      copy(buildPowerRequired = newBuildPower)

  }

}
