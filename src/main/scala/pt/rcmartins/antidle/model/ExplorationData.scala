package pt.rcmartins.antidle.model

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class ExplorationData(
    explorationParties: List[ExplorationParty] = Nil,
)

object ExplorationData {

  implicit val decoder: JsonDecoder[ExplorationData] = DeriveJsonDecoder.gen[ExplorationData]
  implicit val encoder: JsonEncoder[ExplorationData] = DeriveJsonEncoder.gen[ExplorationData]

  val initial: ExplorationData = ExplorationData()

}
