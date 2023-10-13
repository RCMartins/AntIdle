package pt.rcmartins.antidle.game

import pt.rcmartins.antidle.model.WorldData._

object Constants {

  @inline final val u: Long = 10000L

  val MaxTicksCatchUp: Int = 10

  val LayEggSugarCost: Long = 10 * u

  val PlayerGatherSugarClickAmount: Long = 1 * u
  val (defaultTaskCollectSugarTick: Long, defaultTaskCollectSugarSecond: Long) =
    tickAndSeconds((0.1 * u).toLong)

  val (defaultTaskBuildPowerTick: Long, defaultTaskBuildPowerSecond: Long) =
    tickAndSeconds((0.2 * u).toLong)

  val (defaultNestLevelColonyPointsTick: Long, defaultNestLevelColonyPointsSecond: Long) =
    tickAndSeconds((0.1 * u).toLong)

  private def tickAndSeconds(tickValue: Long): (Long, Long) =
    (tickValue, tickValue * TicksPerSecond)

  val NestUpgradeName = "Expand Nest"

  val exponent1_25: IndexedSeq[Double] = (0 to 100).map(Math.pow(1.25, _))

}
