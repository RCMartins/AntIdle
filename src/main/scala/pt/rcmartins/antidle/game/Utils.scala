package pt.rcmartins.antidle.game

import com.raquo.airstream.ownership.OneTimeOwner
import com.raquo.laminar.api.L.{u => _, _}
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model._

import scala.annotation.tailrec
import scala.scalajs.js.timers.setInterval

object Utils {

  var pause: Boolean = false
  val unlocksOwner: SubscriptionManager = new SubscriptionManager

  val worldData: Var[WorldData] = Var(WorldData.initial(System.currentTimeMillis()))
  val antsData: Var[AntsData] = Var(AntsData())
  val antsSignal: Signal[AntsData] = antsData.signal
  val basicResourcesData: Var[BasicResources] = Var(BasicResources())
  val resourcesSignal: Signal[BasicResources] = basicResourcesData.signal
  val nestAttributesData: Var[NestAttributes] = Var(NestAttributes())
  val nestSignal: Signal[NestAttributes] = nestAttributesData.signal
  val chambersSignal: Signal[AllChamberData] = nestSignal.map(_.chambers).distinct
  val unlocksData: Var[Unlocks] = Var(Unlocks())
  val unlocksSignal: Signal[Unlocks] = unlocksData.signal.distinct
  val upgradesData: Var[UpgradesData] = Var(UpgradesData.initial)
  val upgradesSignal: Signal[UpgradesData] = upgradesData.signal.distinct

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
  val messagesSignal: Signal[Seq[String]] = messagesSeq.signal.distinct

//  val lastMessage: Signal[Option[String]] = messagesSeq.signal.map(_.headOption)

  val antTasksUnlockedSignal: Signal[Boolean] =
    unlocksSignal.map(_.tabs.antTasksUnlocked).distinct

  val buildQueueUnlockedSignal: Signal[Boolean] =
    unlocksSignal.map(_.tabs.buildQueueUnlocked).distinct

  val upgradesTabUnlockedSignal: Signal[Boolean] =
    unlocksSignal.map(_.tabs.upgradesTabUnlocked).distinct

  val updateBus: EventBus[Unit] = new EventBus()
  val ticksBus: EventBus[Unit] = new EventBus()
  val actionUpdater: EventBus[AllData => AllData] = new EventBus()

  def initialize(): Unit = {
    val owner = new OneTimeOwner(() => ())
    initializeTicksUpdater(owner)
    initializeActionUpdater(owner)
    UnlockUtils.checkUnlocks(unlocksOwner)
    addMessage("We should collect some sugar to feed our queen.")
    setInterval(200)(updateState())
  }

  def addMessage(message: String): Unit =
    actionUpdater.writer.onNext(_.addMessage(message))

  private def updateState(): Unit =
    if (!pause)
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
        antsData,
        resourcesSignal,
        nestAttributesData,
        unlocksData,
        upgradesData,
        messagesSeq,
      )
      .foreach { case (world, ants, basic, next, unlocks, upgrades, messages) =>
        val ticksToUpdate = world.targetTick - world.currentTick
        if (ticksToUpdate > 0) {
          val allData = AllData.simple(world, ants, basic, next, unlocks, upgrades, messages)

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
        antsData,
        resourcesSignal,
        nestAttributesData,
        unlocksData,
        upgradesData,
        messagesSeq,
      )
      .foreach { case (actionFunc, world, ants, basic, next, unlocks, upgrades, messages) =>
        val allData = AllData.simple(world, ants, basic, next, unlocks, upgrades, messages)
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

  def ifUpgradeReadyToBuyOpt[A](
      upgradeFunc: UpgradesData => UpgradeData
  )(value: => A): Signal[Option[A]] =
    upgradesSignal
      .map { upgrades =>
        val upgrade = upgradeFunc(upgrades)
        upgrade.show && !upgrade.unlocked
      }
      .distinct
      .map {
        case false => None
        case true  => Some(value)
      }

  def hasResourcesSignal(costSignal: Signal[ActionCost]): Signal[Boolean] =
    resourcesSignal.combineWith(costSignal).map { case (resources, cost) =>
      resources.hasResources(cost)
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

  class SubscriptionManager extends Owner {
    private var killableSubscriptions: List[Subscription] = List()

    def track(subscription: Subscription): Unit = {
      killableSubscriptions = subscription :: killableSubscriptions
    }

    def killAll(): Unit = {
      killableSubscriptions.foreach(subscription => if (!subscription.isKilled) subscription.kill())
      killableSubscriptions = List()
    }

  }

}
