package pt.rcmartins.antidle.game

import com.raquo.airstream.ownership.Subscription
import com.raquo.laminar.api.L.{u => _, _}
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.model.UpgradesData.UpgradeType._
import pt.rcmartins.antidle.model._

import scala.util.chaining.scalaUtilChainingOps

object UnlockUtils {

  def checkUnlocks(implicit owner: SubscriptionManager): Unit = {
    unlockSubscription[Long](
      sugarSignal,
      _ >= 5 * u,
      _ =>
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.actions.canLayEggs).setTo(true)),
        )
    )

    unlockSubscription[AntsData](
      antsSignal,
      _.eggsAndLarvae.nonEmpty,
      _ =>
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.resources.showEggs).setTo(true))
        )
    )

    unlockSubscription[Long](
      workersSignal,
      _ >= 1 * u,
      _ => {
        Var.update(
          unlocksData ->
            ((_: Unlocks).modifyAll(_.tabs.antTasksUnlocked, _.resources.showWorkers).setTo(true)),
          antsData -> ((_: AntsData).unlockTask(AntTask.SugarCollector)),
        )
        addMessage("We should give our ants some tasks to do. Let's start with collecting sugars.")
      }
    )

    unlockSubscription[Long](
      antAndBroodCount,
      _ >= 5 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.actions.canBuildNestUpgrade).setTo(true)),
        )
        addMessage("We should upgrade our nest to be able to hold more ants.")
      }
    )

    unlockSubscription[Seq[BuildTask]](
      buildQueueSignal,
      _.nonEmpty,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.tabs.buildQueueUnlocked).setTo(true)),
          antsData -> ((_: AntsData).unlockTask(AntTask.NestBuilder)),
        )
        addMessage("Assign some of our ants as builders.")
      }
    )

    unlockSubscription[Long](
      colonyPointsSignal,
      _ > 0,
      _ => {
        Var.update(
          unlocksData ->
            ((_: Unlocks)
              .modifyAll(_.resources.showColonyPoints, _.tabs.upgradesTabUnlocked)
              .setTo(true)),
          upgradesData ->
            ((_: UpgradesData).show(UnlockQueensChamber, ImproveSugarCollectorTask1)),
        )
        addMessage("We can now upgrade our ants and our nest using colony points.")
      }
    )

    unlockSubscription[UpgradesData](
      upgradesSignal,
      _(ImproveSugarCollectorTask1).unlocked,
      _ =>
        Var.update(
          upgradesData -> ((_: UpgradesData).show(ImproveSugarCollectorTask2)),
        )
    )

    unlockSubscription[UpgradesData](
      upgradesSignal,
      _(ImproveSugarCollectorTask2).unlocked,
      _ =>
        Var.update(
          upgradesData -> ((_: UpgradesData).show(ImproveSugarCollectorTask3)),
        )
    )

    unlockSubscription[UpgradesData](
      upgradesSignal,
      _(UnlockQueensChamber).unlocked,
      _ =>
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.actions.canBuildQueenChamber).setTo(true)),
        )
    )

    unlockSubscription[UpgradesData](
      upgradesSignal,
      upgrades =>
        upgrades(UnlockQueensChamber).unlocked && upgrades(ImproveSugarCollectorTask1).unlocked,
      _ =>
        Var.update(
          upgradesData -> ((_: UpgradesData).show(UnlockFoodStorageChamber)),
        )
    )

    unlockSubscription[UpgradesData](
      upgradesSignal,
      _(UnlockFoodStorageChamber).unlocked,
      _ =>
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.actions.canBuildFoodStorageChamber).setTo(true)),
          upgradesData -> ((_: UpgradesData).show(UnlockExplorerTask)),
        )
    )

    unlockSubscription[UpgradesData](
      upgradesSignal,
      _(UnlockExplorerTask).unlocked,
      _ =>
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.tabs.exploreTabUnlocked).setTo(true)),
          antsData -> ((_: AntsData).unlockTask(AntTask.Explorer)),
        )
    )
  }

  private def unlockSubscription[A](
      valueSignal: Signal[A],
      triggerThreshold: A => Boolean,
      unlock: Long => Unit,
  )(implicit owner: SubscriptionManager): Unit = {
    var subscription: Option[Subscription] = None
    subscription = Some(
      valueSignal
        .withCurrentValueOf(currentTickSignal)
        .foreach { case (value, currentTick) =>
          if (triggerThreshold(value)) {
            unlock(currentTick)
            subscription.foreach(_.kill())
          }
        }
        .tap(owner.track)
    )
  }

}
