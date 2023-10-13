package pt.rcmartins.antidle.utils

import com.raquo.airstream.ownership.OneTimeOwner
import com.raquo.laminar.api.L.{u => _, _}
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model._
import pt.rcmartins.antidle.utils.Utils.messagesSeq

import scala.annotation.tailrec
import scala.scalajs.js.timers.setInterval

object Utils {

  val worldData: Var[WorldData] = Var(WorldData.initial(System.currentTimeMillis()))
  val queensData: Var[QueenData] = Var(QueenData.initial)
  val antsData: Var[AntsData] = Var(AntsData.initial)
  val antsSignal: Signal[AntsData] = antsData.signal
  val basicResourcesData: Var[BasicResources] = Var(BasicResources.initial)
  val resourcesSignal: Signal[BasicResources] = basicResourcesData.signal
  val nestAttributesData: Var[NestAttributes] = Var(NestAttributes.initial)
  val nestSignal: Signal[NestAttributes] = nestAttributesData.signal
  val unlocksData: Var[Unlocks] = Var(Unlocks.initial)
  val unlocksSignal: Signal[Unlocks] = unlocksData.signal.distinct

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

  val sugarSignal: Signal[Long] = resourcesSignal.map(_.sugar).distinct
  val proteinsSignal: Signal[Long] = resourcesSignal.map(_.proteins).distinct
  val waterSignal: Signal[Long] = resourcesSignal.map(_.water).distinct
  val colonyPointsSignal: Signal[Long] = resourcesSignal.map(_.colonyPoints).distinct
  val DNASignal: Signal[Long] = resourcesSignal.map(_.DNA).distinct

  val maxSugar: Signal[Long] = nestAttributesData.signal.map(_.maxSugar).distinct
  val maxEggs: Signal[Long] = nestAttributesData.signal.map(_.maxEggs).distinct
  val maxWorkers: Signal[Long] = nestAttributesData.signal.map(_.maxWorkers).distinct

  val buildQueueSignal: Signal[Seq[BuildTask]] = nestSignal.map(_.buildQueue).distinct
  val maxBuildQueueSignal: Signal[Int] = nestSignal.map(_.maxBuildQueue).distinct

  val messagesSeq: Var[Seq[String]] = Var(Seq.empty)

  val messagesSignal: StrictSignal[Seq[String]] = messagesSeq.signal

//  val lastMessage: Signal[Option[String]] = messagesSeq.signal.map(_.headOption)

  val antTasksUnlockedSignal: Signal[Boolean] = unlocksSignal.map(_.antTasksUnlocked).distinct
  val buildQueueUnlockedSignal: Signal[Boolean] = unlocksSignal.map(_.buildQueueUnlocked).distinct

  val updateBus: EventBus[Unit] = new EventBus()
  val ticksBus: EventBus[Unit] = new EventBus()
  val actionUpdater: EventBus[AllData => AllData] = new EventBus()

  def initialize(): Unit = {
    val owner = new OneTimeOwner(() => ())
    initializeTicksUpdater(owner)
    initializeActionUpdater(owner)
    setInterval(200)(updateState())
    UnlockUtils.checkUnlocks(owner)
    addMessage("We should collect some sugar to feed our queen.")
  }

  def addMessage(message: String): Unit =
    actionUpdater.writer.onNext(_.addMessage(message))

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
        messagesSeq,
      )
      .foreach { case (world, queen, ants, basic, next, unlocks, messages) =>
        val ticksToUpdate = world.targetTick - world.currentTick
        if (ticksToUpdate > 0) {
          val allData = AllData.simple(world, queen, ants, basic, next, unlocks, messages)

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
        messagesSeq,
      )
      .foreach { case (actionFunc, world, queen, ants, basic, next, unlocks, messages) =>
        val allData = AllData.simple(world, queen, ants, basic, next, unlocks, messages)
        actionFunc(allData).updateVars()
      }(owner)

  def giveResources(sugar: Long): Unit =
    actionUpdater.writer.onNext { allData =>
      allData.giveResources(sugar = sugar)
    }

  def ifUnlockedOpt[A](unlocks: Unlocks => Boolean)(value: => A): Signal[Option[A]] =
    unlocksSignal.map(unlocks).distinct.map {
      case false => None
      case true  => Some(value)
    }

  def ifUnlockedOpt[A](signal: Signal[Boolean])(value: => A): Signal[Option[A]] =
    signal.distinct.map {
      case false => None
      case true  => Some(value)
    }

  def ifGreater0[A](compareValue: Long)(func: Long => A): Option[A] =
    if (compareValue == 0) None else Some(func(compareValue))

  def useSignalValue[A](owner: Owner, signal: Signal[A], action: A => Unit): Unit = {
    val eventBus: EventBus[Unit] = new EventBus

    var subscription: Subscription = null
    subscription = eventBus.events
      .sample(signal)
      .foreach { signalValue =>
        action(signalValue)
        subscription.kill()
      }(owner)

    eventBus.writer.onNext(())
  }

}
