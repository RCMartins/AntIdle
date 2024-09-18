package pt.rcmartins.antidle.model

import pt.rcmartins.antidle.game.Constants._
import zio.json._

case class NestAttributes(
    chambers: AllChamberData = AllChamberData(),
    buildQueue: Seq[BuildTask] = Seq.empty,
    dirt: Long = 0,
    deadAnts: Long = 0,
    detritus: Long = 0,
    maxSugar: Long = 1000 * u,
    maxEggs: Long = QueenChamberBonusMaxEggs,
    maxWorkers: Long = NestUpgradeBonusMaxWorkers,
    maxBuildQueue: Int = 1,
)

object NestAttributes {

  implicit val decoder: JsonDecoder[NestAttributes] = DeriveJsonDecoder.gen[NestAttributes]
  implicit val encoder: JsonEncoder[NestAttributes] = DeriveJsonEncoder.gen[NestAttributes]

  val initial: NestAttributes = NestAttributes()

}
