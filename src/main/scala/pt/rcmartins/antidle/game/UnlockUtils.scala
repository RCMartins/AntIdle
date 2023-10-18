package pt.rcmartins.antidle.game

import com.raquo.airstream.ownership.Subscription
import com.raquo.laminar.api.L.{u => _, _}
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.model.{AntTask, AntsData, BuildTask, Unlocks, UpgradesData}

object UnlockUtils {

  // TODO this does not work properly when the game is loaded from a save
  def checkUnlocks(owner: Owner): Unit = {
    unlockSubscription[Long](
      sugarSignal,
      _ >= 5 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.actions.canLayEggs).setTo(true)),
        )
      }
    )(owner)

    // TODO build storage chamber
//    unlockSubscription[Long](
//      sugarSignal,
//      _ >= 250 * u,
//      _ => {
//        Var.update(
//          unlocksData -> ((_: Unlocks).copy(canBuildStorageChamber = true)),
//        )
//      }
//    )(owner)

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
    )(owner)

    unlockSubscription[Long](
      antAndBroodCount,
      _ >= 5 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).modify(_.actions.canBuildNestUpgrade).setTo(true)),
        )
        addMessage("We should upgrade our nest to be able to hold more ants.")
      }
    )(owner)

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
    )(owner)

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
            ((_: UpgradesData)
              .modifyAll(_.queensChamber.show, _.improveSugarCollectorTask.show)
              .setTo(true)),
        )
        addMessage("We can now upgrade our ants and our nest using colony points.")
      }
    )(owner)
  }

  private def unlockSubscription[A](
      valueSignal: Signal[A],
      triggerThreshold: A => Boolean,
      unlock: Long => Unit,
  )(owner: Owner): Unit = {
    var subscription: Subscription = null
    subscription = valueSignal
      .withCurrentValueOf(currentTickSignal)
      .foreach { case (value, currentTick) =>
        if (triggerThreshold(value)) {
          unlock(currentTick)
          subscription.kill()
        }
      }(owner)
  }

}
