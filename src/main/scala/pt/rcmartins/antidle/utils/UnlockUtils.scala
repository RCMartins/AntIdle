package pt.rcmartins.antidle.utils

import com.raquo.airstream.ownership.Subscription
import com.raquo.laminar.api.L.{u => _, _}
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.{AntTask, AntsData, NestAttributes, Unlocks}
import pt.rcmartins.antidle.utils.Utils._

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

  def checkUnlocks(owner: Owner): Unit = {
    unlockSubscription[Long](
      sugarsSignal,
      _ >= 5 * u,
      _ => {
        addMessage("We can now feed our starving queen!")
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
      }
    )(owner)

    unlockSubscription[Long](
      workersSignal,
      _ >= 5 * u,
      _ => {
        Var.update(
          unlocksData -> ((_: Unlocks).copy(canBuildNest = true)),
        )
      }
    )(owner)

    unlockSubscription[NestAttributes](
      nestSignal,
      _.nestLevel >= 1,
      _ => {
        Var.update(
          antsData -> ((_: AntsData).unlockTask(AntTask.NestBuilder)),
        )
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
