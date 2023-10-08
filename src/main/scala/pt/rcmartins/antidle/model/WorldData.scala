package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.model.WorldData._

case class WorldData(
    tick: Long,
    lastUpdateTime: Long,
) {

  def getPassedTicks(currentTime: Long): Long = {
    val delta = currentTime - lastUpdateTime
    val passedTicks = delta / millsPerTick
    passedTicks
  }

  def tickBump(): WorldData =
    copy(
      tick = tick + 1,
      lastUpdateTime = lastUpdateTime + millsPerTick,
    )

}

object WorldData {

  val TicksPerSecond: Long = 5
  val millsPerTick: Long = 1000 / TicksPerSecond

  def initial(currentTime: Long): WorldData =
    WorldData(
      tick = 0,
      lastUpdateTime = currentTime,
    )

}
