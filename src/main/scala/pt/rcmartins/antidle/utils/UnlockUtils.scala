package pt.rcmartins.antidle.utils

import com.raquo.airstream.ownership.Subscription
import com.raquo.laminar.api.L._
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.model.{AntBrood, AntTask, AntsData, Unlocks, WorldData}
import pt.rcmartins.antidle.utils.Utils._

object UnlockUtils {

  /*
  Player tasks:
    1) Manually collect 10 sugars
    2) Manually lay 1 egg
    2.1) Wait until egg hatches -> Unlock tasks tab
    3) Manually collect sugars until laying 5 eggs and having 5 workers

   */

  def checkUnlocks(owner: Owner): Unit = {
    unlockSubscription(
      sugarsSignal,
      10,
      _ => {
        addMessage("We can now feed our starving queen!")
        Var.update(
          unlocksData -> ((u: Unlocks) => u.copy(canLayEggs = true)),
        )
      }
    )(owner)

    unlockSubscription(
      workersSignal,
      1,
      _ => {
        Var.update(
          unlocksData -> ((u: Unlocks) => u.copy(antTasksUnlocked = true)),
          antsData -> ((a: AntsData) => a.unlockTask(AntTask.SugarCollector)),
        )
      }
    )(owner)

    unlockSubscription(
      workersSignal,
      5,
      _ => {
        Var.update(
          unlocksData -> ((u: Unlocks) => u.copy(canUpgradeNest = true)),
        )
      }
    )(owner)
  }

  private def unlockSubscription(
      valueSignal: Signal[Long],
      triggerThreshold: Long,
      unlock: Long => Unit,
  )(owner: Owner): Unit = {
    var subscription: Subscription = null
    subscription = valueSignal
      .withCurrentValueOf(currentTickSignal)
      .foreach { case (value, currentTick) =>
        if (value >= triggerThreshold) {
          unlock(currentTick)
          subscription.kill()
        }
      }(owner)
  }

}
