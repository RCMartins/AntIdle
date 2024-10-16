package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class ExplorationData(
    maxExplorationsParties: Int = Constants.InitialMaxExplorationParties,
    explorationParties: List[ExplorationParty] = Nil,
)

object ExplorationData {

  implicit val decoder: JsonDecoder[ExplorationData] = DeriveJsonDecoder.gen[ExplorationData]
  implicit val encoder: JsonEncoder[ExplorationData] = DeriveJsonEncoder.gen[ExplorationData]

  val initial: ExplorationData = ExplorationData()

}
