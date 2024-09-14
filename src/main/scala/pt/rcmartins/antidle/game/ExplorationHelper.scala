package pt.rcmartins.antidle.game

import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.{AllData, AntTask, ExplorationParty}

import scala.util.Random

object ExplorationHelper {

  def exploreBonus(
      allData: AllData,
      exploration: ExplorationParty,
  ): AllData = {
    val amountOfWorkersU = exploration.amountWorkers

    val random = new Random(allData.world.saveSeed + allData.world.currentTick)

    val maxValue = 100
    val randomValue = random.nextInt(maxValue) + 1

    def withReturnedAnts: AllData =
      allData.modify(_.ants).using(_.removeFromTask(AntTask.Explorer, amountOfWorkersU))

    randomValue match {
      case r if r >= 1 && r <= 50 =>
        val sugarFound = dices(random, amountOfWorkersU / u, 20).sum * u
        withReturnedAnts
          .giveResources(sugar = sugarFound)
          .addMessage(
            s"Exploration: Found ${UINumbersUtils.prettyNumberStr(sugarFound)} sugar!"
          )
      case r if r >= 51 && r <= 100 =>
        withReturnedAnts.addMessage("Exploration: Found nothing...")
    }
  }

  def dice(random: Random, maxValue: Long): Long = random.nextLong(maxValue) + 1L

  def dices(random: Random, amount: Long, maxValue: Long): Seq[Long] =
    (1 to amount.toInt).map(_ => dice(random, maxValue))

}
