package pt.rcmartins.antidle.utils

import com.raquo.laminar.api.L.{u => _, _}
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.{ActionCost, AntBrood, AntTask}
import pt.rcmartins.antidle.utils.Utils._

object Actions {

  def reduceAntTask(antTask: AntTask): Unit =
    actionUpdater.writer.onNext { allData =>
      allData
        .modify(_.ants.tasks)
        .using(_.map {
          case (task, amount) if task == antTask => (task, amount - 1L * u)
          case other                             => other
        })
    }

  def incrementAntTask(antTask: AntTask): Unit =
    actionUpdater.writer.onNext { allData =>
      allData
        .modify(_.ants.tasks)
        .using(_.map {
          case (task, amount) if task == antTask => (task, amount + 1L * u)
          case other                             => other
        })
    }

  val layEggActionCost: Signal[ActionCost] =
    Val(ActionCost(sugars = Constants.EggSugarCost))

  val layEggActionEnabled: Signal[Boolean] =
    sugarsSignal
      .combineWith(eggsCountSignal, maxEggs, antAndBroodCount, maxWorkers)
      .map { case (sugars, eggsCount, maxEggs, antCount, maxAnt) =>
        sugars >= Constants.EggSugarCost && eggsCount < maxEggs && antCount < maxAnt
      }

  def layEggAction(): Unit =
    actionUpdater.writer.onNext { allData =>
      allData
        .modify(_.basicResources.sugars)
        .using(_ - Constants.EggSugarCost)
        .modify(_.ants.eggsAndLarvae)
        .using(_ :+ AntBrood.Egg(allData.world.tick))
        .modify(_.unlocks.layedFirstEgg)
        .setTo(true)
    }

}
