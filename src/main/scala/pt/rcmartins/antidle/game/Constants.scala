package pt.rcmartins.antidle.game

import pt.rcmartins.antidle.model.Chamber.ChamberType

object Constants {

  // UI Constants

  val ImportModalId: String = "importModal"
  val AlertTimeout: Int = 5000

  // Time Constants

  val TicksPerSecond: Long = 5
  val millsPerTick: Long = 1000 / TicksPerSecond
  val MaxTicksCatchUp: Int = 10

  private def tickAndSeconds(perSecondValue: Long): (Long, Long) =
    (perSecondValue / TicksPerSecond, perSecondValue)

  def ExplorationTimeTicks: Long = 15 * TicksPerSecond

  // Other Constants

  @inline final val u: Long = 10000L

  val MaxExponent = 100
  val exponent0_95: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(0.95, _))
  val exponent1_14: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(1.14, _))
  val exponent1_25: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(1.25, _))
  val exponent1_75: IndexedSeq[Double] = (0 to MaxExponent).map(Math.pow(1.75, _))

  val LayEggSugarCost: Long = 10 * u

  val PlayerGatherSugarClickAmount: Long = 1 * u
  val (defaultTaskCollectSugarTick: Long, defaultTaskCollectSugarSecond: Long) =
    tickAndSeconds((0.4 * u).toLong)

  val (defaultTaskBuildPowerTick: Long, defaultTaskBuildPowerSecond: Long) =
    tickAndSeconds((1.0 * u).toLong)

  val (defaultNestLevelColonyPointsTick: Long, defaultNestLevelColonyPointsSecond: Long) =
    tickAndSeconds((0.01 * u).toLong)

  val (antsSugarUpkeepTick: Long, antsSugarUpkeepSecond: Long) =
    tickAndSeconds((0.2 * u).toLong)

  val NestUpgradeBonusMaxWorkers: Long = 5 * u
  val QueenChamberBonusMaxEggs: Long = 2 * u
  val FoodStorageChamberBonusMaxSugar: Long = 1000 * u

  val AntDeathRiskThreshold: Long = 10 * u

  val NestUpgradeName = "Expand Nest"
  val QueenChamberName = "Queen's Chamber"
  val StorageChamberName = "Food Storage Chamber"
  val NurseryChamberName = "Nursery Chamber"

  def getChamberBuildPower(chamberType: ChamberType, level: Int): Long = {
    def cost(base: Long, multiplier: IndexedSeq[Double]): Long =
      (base * multiplier(level)).toLong

    chamberType match {
      case ChamberType.Nest        => cost(20 * u, exponent1_75)
      case ChamberType.Queen       => cost(25 * u, exponent1_25)
      case ChamberType.FoodStorage => cost(50 * u, exponent1_14)
      case ChamberType.Nursery     => 0 // TODO
    }
  }

  val MaxExplorationParties: Int = 1

}
