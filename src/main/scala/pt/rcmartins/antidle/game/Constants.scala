package pt.rcmartins.antidle.game

import pt.rcmartins.antidle.model.WorldData._

import scala.util.Random

object Constants {

  @inline final val u: Long = 10000L

  private val MaxExponent = 100
  val exponent0_95: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(0.95, _))
  val exponent1_25: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(1.25, _))
  val exponent1_75: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(1.75, _))

  val MaxTicksCatchUp: Int = 10

  val LayEggSugarCost: Long = 10 * u

  val PlayerGatherSugarClickAmount: Long = 1 * u
  val (defaultTaskCollectSugarTick: Long, defaultTaskCollectSugarSecond: Long) =
    tickAndSeconds((0.4 * u).toLong)

  val (defaultTaskBuildPowerTick: Long, defaultTaskBuildPowerSecond: Long) =
    tickAndSeconds((1.0 * u).toLong)

  val (defaultNestLevelColonyPointsTick: Long, defaultNestLevelColonyPointsSecond: Long) =
    tickAndSeconds((0.02 * u).toLong)

  val (antsSugarUpkeepTick: Long, antsSugarUpkeepSecond: Long) =
    tickAndSeconds((0.2 * u).toLong)

  val NestUpgradeBaseCost: Long = 20 * u
  val NestUpgradeMultiplier: IndexedSeq[Double] = exponent1_75
  val NestUpgradeBonusMaxWorkers: Long = 5 * u

  val QueenChamberBonusMaxEggs: Long = 2 * u

  val AntDeathRiskThreshold: Long = 10 * u

  private def tickAndSeconds(perSecondValue: Long): (Long, Long) =
    (perSecondValue / TicksPerSecond, perSecondValue)

  val NestUpgradeName = "Expand Nest"
  val QueenChamberName = "Queen Chamber"

  def antDeathThisTick(seed: Long, sugarCumulativeDebt: Long): Boolean =
    if (sugarCumulativeDebt < AntDeathRiskThreshold) false
    else if (sugarCumulativeDebt >= AntDeathRiskThreshold * (MaxExponent - 1)) true
    else {
      val exponent = (sugarCumulativeDebt / AntDeathRiskThreshold).toInt
      val chance = exponent0_95(exponent)
      new Random(seed).nextDouble() > chance
    }

}
