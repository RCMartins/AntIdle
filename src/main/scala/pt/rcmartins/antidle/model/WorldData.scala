package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants
import pt.rcmartins.antidle.game.Constants.millsPerTick
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

  // Constants.MaxTicksStored + 1 so it considers the current tick that is being processed
  def addToTargetTick(passedTicks: Long): WorldData =
    copy(targetTick =
      currentTick + Math.min(Constants.MaxTicksStored + 1, (targetTick - currentTick) + passedTicks)
    )

}

object WorldData {

  implicit val decoder: JsonDecoder[WorldData] = DeriveJsonDecoder.gen[WorldData]
  implicit val encoder: JsonEncoder[WorldData] = DeriveJsonEncoder.gen[WorldData]

  def initial(currentTime: Long): WorldData =
    WorldData(
      saveSeed = Random.nextLong(),
      currentTick = 0,
      targetTick = 0,
      lastUpdateTime = currentTime,
    )

}
