package pt.rcmartins.antidle.utils

import com.raquo.airstream.ownership.OneTimeOwner
import com.raquo.laminar.api.L.{u => _, _}
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model._

import scala.annotation.tailrec
import scala.scalajs.js.timers.setInterval

object Utils {

  val worldData: Var[WorldData] = Var(WorldData.initial(System.currentTimeMillis()))
  val queensData: Var[QueenData] = Var(QueenData.initial)
  val antsData: Var[AntsData] = Var(AntsData.initial)
  val antsSignal = antsData.signal
  val basicResourcesData: Var[BasicResources] = Var(BasicResources.initial)
  val resourcesSignal = basicResourcesData.signal
  val nestAttributesData: Var[NestAttributes] = Var(NestAttributes.initial)
  val unlocksData: Var[Unlocks] = Var(Unlocks.initial)
  val unlocksSignal = unlocksData.signal.distinct

  val currentTickSignal: Signal[Long] = worldData.signal.map(_.currentTick).distinct

  val workersSignal: Signal[Long] = antsSignal.map(_.workers).distinct
  val eggsCountSignal: Signal[Long] =
    antsSignal.map(_.eggsAndLarvae.count(_.isEgg) * u).distinct

  val larvaeCountSignal: Signal[Long] =
    antsSignal.map(_.eggsAndLarvae.count(_.isLarvae) * u).distinct

  val pupaeCountSignal: Signal[Long] =
    antsSignal.map(_.eggsAndLarvae.count(_.isPupae) * u).distinct

  val antAndBroodCount: Signal[Long] =
    workersSignal
      .combineWith(eggsCountSignal, larvaeCountSignal, pupaeCountSignal)
      .map { case (workers, eggs, larvae, pupae) =>
        workers + eggs + larvae + pupae
      }
      .distinct

  val unlockedAntTasks: Signal[Seq[AntTask]] = antsSignal.map(_.tasks.map(_._1)).distinct
  val idleWorkersCountSignal: Signal[Long] = antsData.signal.map(_.idleWorkersCount).distinct

  val sugarsSignal: Signal[Long] = resourcesSignal.map(_.sugars).distinct
  val proteinsSignal: Signal[Long] = resourcesSignal.map(_.proteins).distinct
  val waterSignal: Signal[Long] = resourcesSignal.map(_.water).distinct
  val colonyPointsSignal: Signal[Long] = resourcesSignal.map(_.colonyPoints).distinct
  val DNASignal: Signal[Long] = resourcesSignal.map(_.DNA).distinct

  val maxSugars: Signal[Long] = nestAttributesData.signal.map(_.maxSugars).distinct
  val maxEggs: Signal[Long] = nestAttributesData.signal.map(_.maxEggs).distinct
  val maxWorkers: Signal[Long] = nestAttributesData.signal.map(_.maxWorkers).distinct

  private val MaxMessages = 10
  val messages: Var[Seq[String]] = Var(Seq.empty)

  def addMessage(message: String): Unit =
    messages.update(oldMessages => (message +: oldMessages).take(MaxMessages))

  val lastMessage: Signal[Option[String]] = messages.signal.map(_.headOption)

  val antTasksUnlockedSignal: Signal[Boolean] = unlocksSignal.map(_.antTasksUnlocked).distinct

  def initialize(): Unit = {
    val owner = new OneTimeOwner(() => ())
    initializeTicksUpdater(owner)
    initializeActionUpdater(owner)
    setInterval(200)(updateState())
    addMessage("The queen needs help collecting food!")
    UnlockUtils.checkUnlocks(owner)
  }

  val updateBus: EventBus[Unit] = new EventBus()
  val ticksBus: EventBus[Unit] = new EventBus()
  val actionUpdater: EventBus[AllData => AllData] = new EventBus()

  private def updateState(): Unit =
    updateBus.writer.onNext(())

  private def initializeTicksUpdater(owner: Owner): Unit = {
    updateBus.events
      .sample(worldData)
      .foreach { world =>
        val passedTicks = world.getPassedTicks(System.currentTimeMillis())
        worldData.update(
          _.modify(_.targetTick)
            .using(_ + passedTicks)
            .modify(_.lastUpdateTime)
            .using(_ + WorldData.millsPerTick * passedTicks)
        )
        ticksBus.writer.onNext(())
      }(owner)

    ticksBus.events
      .sample(
        worldData,
        queensData,
        antsData,
        resourcesSignal,
        nestAttributesData,
        unlocksData,
      )
      .foreach { case (world, queen, ants, basic, next, unlocks) =>
        val ticksToUpdate = world.targetTick - world.currentTick
        if (ticksToUpdate > 0) {
          val allData = AllData(world, queen, ants, basic, next, unlocks)

          @tailrec
          def loop(ticksLeft: Long, allData: AllData): AllData =
            if (ticksLeft <= 0)
              allData
            else
              loop(ticksLeft - 1, TickUpdater.updateAllData(allData))

          loop(Math.min(ticksToUpdate, Constants.MaxTicksCatchUp), allData).updateVars()
        }
      }(owner)
  }

  private def initializeActionUpdater(owner: Owner): Unit =
    actionUpdater.events
      .withCurrentValueOf(
        worldData,
        queensData,
        antsData,
        resourcesSignal,
        nestAttributesData,
        unlocksData,
      )
      .foreach { case (actionFunc, world, queen, ants, basic, next, unlocks) =>
        val allData = AllData(world, queen, ants, basic, next, unlocks)
        actionFunc(allData).updateVars()
      }(owner)

  def giveResources(sugars: Long): Unit =
    actionUpdater.writer.onNext { allData =>
      allData
        .modify(_.basicResources.sugars)
        .usingIf(sugars > 0)(current =>
          Math.min(current + sugars, allData.nestAttributes.maxSugars)
        )
    }

}
