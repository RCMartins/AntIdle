package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.model.WorldData._
import zio.json._

import scala.util.Random

case class WorldData(
    saveSeed: Long,
    currentTick: Long,
    targetTick: Long,
    lastUpdateTime: Long,
) {

  def getPassedTicks(currentTime: Long): Long = {
    val delta = currentTime - lastUpdateTime
    val passedTicks = delta / millsPerTick
    passedTicks
  }

}

object WorldData {

  implicit val decoder: JsonDecoder[WorldData] = DeriveJsonDecoder.gen[WorldData]
  implicit val encoder: JsonEncoder[WorldData] = DeriveJsonEncoder.gen[WorldData]

  val TicksPerSecond: Long = 5
  val millsPerTick: Long = 1000 / TicksPerSecond

  def initial(currentTime: Long): WorldData =
    WorldData(
      saveSeed = Random.nextLong(),
      currentTick = 0,
      targetTick = 0,
      lastUpdateTime = currentTime,
    )

}
