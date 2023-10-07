package pt.rcmartins.antidle.utils

import com.raquo.airstream.state.Var
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.model._
import pt.rcmartins.antidle.utils.Utils._
import zio.json._

case class AllData(
    world: WorldData,
    queens: QueenData,
    ants: AntsData,
    basicResources: BasicResources,
    nestAttributes: NestAttributes,
    unlocks: Unlocks,
) {

  def giveResources(sugars: Long): AllData =
    this
      .modify(_.basicResources.sugars)
      .usingIf(sugars > 0)(current => Math.min(current + sugars, nestAttributes.maxSugars))

  def updateVars(): Unit =
    Var.set(
      worldData -> world,
      queensData -> queens,
      antsData -> ants,
      basicResourcesData -> basicResources,
      nestAttributesData -> nestAttributes,
      unlocksData -> unlocks,
    )

}

object AllData {

  implicit val decoder: JsonDecoder[AllData] = DeriveJsonDecoder.gen[AllData]
  implicit val encoder: JsonEncoder[AllData] = DeriveJsonEncoder.gen[AllData]

}
