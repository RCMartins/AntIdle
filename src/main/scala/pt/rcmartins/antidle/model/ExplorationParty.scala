package pt.rcmartins.antidle.model

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

import scala.util.chaining.scalaUtilChainingOps

case class ExplorationParty(
    amountWorkers: Long,
    startingTick: Long,
    targetTick: Long,
)

object ExplorationParty {

  implicit val decoder: JsonDecoder[ExplorationParty] = DeriveJsonDecoder.gen[ExplorationParty]
  implicit val encoder: JsonEncoder[ExplorationParty] = DeriveJsonEncoder.gen[ExplorationParty]

  def create(amountWorkers: Long, currentTick: Long, explorationTimeTicks: Long): ExplorationParty =
    ExplorationParty(
      amountWorkers = amountWorkers,
      startingTick = currentTick,
      targetTick = currentTick + explorationTimeTicks,
    )

}
