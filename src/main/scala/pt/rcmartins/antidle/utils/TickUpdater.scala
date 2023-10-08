package pt.rcmartins.antidle.utils

import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.model.AntBrood
import pt.rcmartins.antidle.utils.Utils.AllData

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
          val newWorkers = updatedEggsAndLarvae.count(_.isLeft)

          allData
            .modify(_.ants.eggsAndLarvae)
            .setTo(updatedEggsAndLarvae.flatMap(_.toOption))
            .modify(_.ants.workers)
            .usingIf(newWorkers > 0)(_ + newWorkers)
            .modify(_.unlocks.bornFirstWorker)
            .setToIf(newWorkers > 0)(true)
        },
      )

    updates
      .foldLeft(initial) { case (allData, update) =>
        update(allData)
      }
      .updateVars()
  }

}
