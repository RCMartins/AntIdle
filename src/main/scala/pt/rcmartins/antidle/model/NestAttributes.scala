package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.Chamber._

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
