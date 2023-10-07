package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.Chamber._
import zio.json._

case class NestAttributes(
    chambers: Seq[Chamber],
    dirt: Long,
    deadAnts: Long,
    detritus: Long,
    maxSugars: Long,
    maxEggs: Long,
    maxWorkers: Long,
)

object NestAttributes {

  implicit val decoder: JsonDecoder[NestAttributes] = DeriveJsonDecoder.gen[NestAttributes]
  implicit val encoder: JsonEncoder[NestAttributes] = DeriveJsonEncoder.gen[NestAttributes]

  val initial: NestAttributes =
    NestAttributes(
      chambers = Seq(QueenChamber(1)),
      dirt = 0,
      deadAnts = 0,
      detritus = 0,
      maxSugars = 1000 * u,
      maxEggs = 2 * u,
      maxWorkers = 5 * u,
    )

}
