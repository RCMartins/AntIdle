package pt.rcmartins.antidle.model

import com.raquo.airstream.state.Var
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.model.AllData.MaxMessages
import pt.rcmartins.antidle.game.Utils._
import zio.json._

case class AllData(
    version: Int = AllData.CurrentVersion,
    world: WorldData,
    queens: QueenData,
    ants: AntsData,
    basicResources: BasicResources,
    nestAttributes: NestAttributes,
    unlocks: Unlocks,
    upgrades: UpgradesData,
    messages: Seq[String],
) {

  def giveResources(sugar: Long = 0, colonyPoints: Long = 0): AllData =
    this
      .modify(_.basicResources.sugar)
      .usingIf(sugar > 0)(current => Math.min(current + sugar, nestAttributes.maxSugar))
      .modify(_.basicResources.colonyPoints)
      .usingIf(colonyPoints > 0)(_ + colonyPoints)

  def updateVars(): Unit =
    Var.set(
      worldData -> world,
      queensData -> queens,
      antsData -> ants,
      basicResourcesData -> basicResources,
      nestAttributesData -> nestAttributes,
      unlocksData -> unlocks,
      upgradesData -> upgrades,
      messagesSeq -> messages,
    )

  def addMessage(message: String): AllData =
    this.modify(_.messages).using(oldMessages => (message +: oldMessages).take(MaxMessages))

}

object AllData {

  implicit val decoder: JsonDecoder[AllData] = DeriveJsonDecoder.gen[AllData]
  implicit val encoder: JsonEncoder[AllData] = DeriveJsonEncoder.gen[AllData]

  val CurrentVersion = 1

  private val MaxMessages = 10

  def simple(
      world: WorldData,
      queen: QueenData,
      ants: AntsData,
      basic: BasicResources,
      next: NestAttributes,
      unlocks: Unlocks,
      upgrades: UpgradesData,
      messages: Seq[String]
  ): AllData =
    AllData(
      version = CurrentVersion,
      world = world,
      queens = queen,
      ants = ants,
      basicResources = basic,
      nestAttributes = next,
      unlocks = unlocks,
      upgrades = upgrades,
      messages = messages,
    )

}
