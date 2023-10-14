package pt.rcmartins.antidle.game

import com.raquo.airstream.ownership.Subscription
import com.raquo.laminar.api.L.{u => _, _}
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.model.{AntTask, AntsData, BuildTask, Unlocks}

object UnlockUtils {

  /*
  Player tasks:
    1) Manually collect 10 sugars
    2) Manually lay 1 egg
      2.1) Wait until egg hatches -> Unlock tasks tab
      2.2) Start collecting sugars passively using workers
    3) Collect sugars / laying eggs until having a total of 5 workers -> Unlock upgrades tab
    4) Upgrade nest (requires building power?)
    5) More ants -> More sugars -> More upgrades -> More ants -> Loop
   */

  // TODO this does not work properly when the game is loaded from a save
  def checkUnlocks(owner: Owner): Unit = {
    unlockSubscription[Long](
      sugarSignal,
      _ >= 5 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).copy(canLayEggs = true)),
        )
      }
    )(owner)

    unlockSubscription[Long](
      workersSignal,
      _ >= 1 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).copy(antTasksUnlocked = true)),
          antsData -> ((_: AntsData).unlockTask(AntTask.SugarCollector)),
        )
        addMessage("We should give our ants some tasks to do. Let's start with collecting sugars.")
      }
    )(owner)

    unlockSubscription[Long](
      workersSignal,
      _ >= 5 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).copy(canBuildNestUpgrade = true)),
        )
        addMessage("We should upgrade our nest to be able to hold more ants.")
      }
    )(owner)

    unlockSubscription[Seq[BuildTask]](
      buildQueueSignal,
      _.nonEmpty,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).copy(buildQueueUnlocked = true)),
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
          unlocksData -> ((_: Unlocks).copy(showColonyPointsResource = true)),
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
