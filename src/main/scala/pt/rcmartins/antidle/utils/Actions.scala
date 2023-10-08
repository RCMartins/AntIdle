package pt.rcmartins.antidle.utils

import com.raquo.laminar.api.L.Signal
import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants
import pt.rcmartins.antidle.model.AntBrood
import pt.rcmartins.antidle.utils.Utils._

object Actions {

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
