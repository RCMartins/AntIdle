package pt.rcmartins.antidle.model

import com.raquo.airstream.state.Var
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.model.AllData.MaxMessages

case class AllData(
    version: Int = AllData.CurrentVersion,
    world: WorldData,
    ants: AntsData,
    basicResources: BasicResources,
    nestAttributes: NestAttributes,
    unlocks: Unlocks,
    upgrades: UpgradesData,
    exploration: ExplorationData,
    statistics: StatisticsData,
    messages: Seq[String],
) {

  def currentTick: Long = world.currentTick

  def giveResources(
      sugar: Long = 0,
      colonyPoints: Long = 0,
  ): AllData = {
    val sugarGained: Long =
      Math.min(nestAttributes.maxSugar, basicResources.sugar + sugar) - basicResources.sugar

    this
      .modify(_.basicResources.sugar)
      .usingIf(sugarGained > 0)(_ + sugarGained)
      .modify(_.statistics.sugarU)
      .usingIf(sugarGained > 0)(_.addValue(sugarGained))
      .modify(_.basicResources.colonyPoints)
      .usingIf(colonyPoints > 0)(_ + colonyPoints) // TODO change when colonyPoints are limited...
      .modify(_.statistics.colonyPointsU)
      .usingIf(colonyPoints > 0)(_.addValue(colonyPoints))
  }

  def spendResources(actionCost: ActionCost): AllData =
    copy(
      basicResources = basicResources.spendResources(actionCost),
    )

  def purchaseIfPossible(actionCost: ActionCost)(updateIfPossibleF: AllData => AllData): AllData =
    if (basicResources.hasResources(actionCost) && ants.hasIdleWorkersForCost(actionCost))
      updateIfPossibleF(spendResources(actionCost))
    else
      this

  def updateVars(): Unit =
    Var.set(
      worldData -> world,
      antsData -> ants,
      basicResourcesData -> basicResources,
      nestAttributesData -> nestAttributes,
      unlocksData -> unlocks,
      upgradesData -> upgrades,
      explorationData -> exploration,
      statisticsData -> statistics,
      messagesSeq -> messages,
    )

  def addMessage(message: String): AllData =
    this.modify(_.messages).using(oldMessages => (message +: oldMessages).take(MaxMessages))

}

object AllData {

  val CurrentVersion = 1

  private val MaxMessages = 10

  val initial: AllData =
    AllData(
      world = WorldData.initial(System.currentTimeMillis()),
      ants = AntsData.initial,
      basicResources = BasicResources.initial,
      nestAttributes = NestAttributes.initial,
      unlocks = Unlocks.initial,
      upgrades = UpgradesData.initial,
      exploration = ExplorationData.initial,
      statistics = StatisticsData.initial,
      messages = Seq.empty,
    )

  def simple(
      world: WorldData,
      ants: AntsData,
      basic: BasicResources,
      next: NestAttributes,
      unlocks: Unlocks,
      upgrades: UpgradesData,
      exploration: ExplorationData,
      statistics: StatisticsData,
      messages: Seq[String]
  ): AllData =
    AllData(
      version = CurrentVersion,
      world = world,
      ants = ants,
      basicResources = basic,
      nestAttributes = next,
      unlocks = unlocks,
      upgrades = upgrades,
      exploration = exploration,
      statistics = statistics,
      messages = messages,
    )

}
