package pt.rcmartins.antidle.game

import pt.rcmartins.antidle.model.WorldData._

object Constants {

  @inline final val u: Long = 1000L

  val MaxTicksCatchUp: Int = 10

  val EggSugarCost: Long = 10 * u

  val PlayerGatherSugarClickAmount: Long = 1 * u
  val DefaultTaskCollectSugarTick: Long = (0.1 * u).toLong
  val DefaultTaskCollectSugarSecond: Long = DefaultTaskCollectSugarTick * TicksPerSecond

  val DefaultTaskBuildPowerTick: Long = (0.2 * u).toLong
  val DefaultTaskBuildPowerSecond: Long = DefaultTaskBuildPowerTick * TicksPerSecond

}
