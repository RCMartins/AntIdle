package pt.rcmartins.antidle.utils

import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.{AntBrood, AntTask}

object TickUpdater {

  def updateTick(initial: AllData): Unit = {
    val updates: Seq[AllData => AllData] =
      Seq[AllData => AllData](
        allData => allData.modify(_.world).using(_.tickBump()),
        allData => {
          val currentTick = allData.world.tick
          val unlocks = allData.unlocks

          val updatedEggsAndLarvae: Seq[Either[Unit, AntBrood]] =
            allData.ants.eggsAndLarvae.map {
              case AntBrood.Egg(tick)
                  if !unlocks.larvaeUnlocked && currentTick >= tick + AntBrood.defaultTicksToLarva =>
                Left(())
              case other =>
                Right(other)
            }
          val newWorkers = updatedEggsAndLarvae.count(_.isLeft) * u

          allData
            .modify(_.ants.eggsAndLarvae)
            .setTo(updatedEggsAndLarvae.flatMap(_.toOption))
            .modify(_.ants.workers)
            .usingIf(newWorkers > 0)(_ + newWorkers)
            .modify(_.unlocks.bornFirstWorker)
            .setToIf(newWorkers > 0)(true)
        },
        allData => {
          def countWorkers(task: AntTask): Long =
            allData.ants.tasks.find(_._1 == task).map(_._2 / u).getOrElse(0L)

          val sugarWorkers: Long = countWorkers(AntTask.SugarCollector)
          allData.giveResources(
            sugars = sugarWorkers * Constants.DefaultTaskCollectSugarTick,
          )
        }
      )

    updates
      .foldLeft(initial) { case (allData, update) =>
        update(allData)
      }
      .updateVars()
  }

}
