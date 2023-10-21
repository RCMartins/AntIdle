package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants._
import zio.json._

case class NestAttributes(
    chambers: AllChamberData,
    buildQueue: Seq[BuildTask],
    dirt: Long,
    deadAnts: Long,
    detritus: Long,
    maxSugar: Long,
    maxEggs: Long,
    maxWorkers: Long,
    maxBuildQueue: Int,
)

object NestAttributes {

  implicit val decoder: JsonDecoder[NestAttributes] = DeriveJsonDecoder.gen[NestAttributes]
  implicit val encoder: JsonEncoder[NestAttributes] = DeriveJsonEncoder.gen[NestAttributes]

  val initial: NestAttributes =
    NestAttributes(
      chambers = AllChamberData(),
      buildQueue = Seq.empty,
      dirt = 0,
      deadAnts = 0,
      detritus = 0,
      maxSugar = 1000 * u,
      maxEggs = QueenChamberBonusMaxEggs,
      maxWorkers = NestUpgradeBonusMaxWorkers,
      maxBuildQueue = 1,
    )

}
