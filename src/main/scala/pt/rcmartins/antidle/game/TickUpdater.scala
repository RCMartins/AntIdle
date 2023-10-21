package pt.rcmartins.antidle.game

import com.softwaremill.quicklens.ModifyPimp
import pt.rcmartins.antidle.game.Constants.u
import pt.rcmartins.antidle.model.{AllData, AntBrood, AntTask, BuildTask}

import scala.util.chaining.scalaUtilChainingOps

object TickUpdater {

  def updateAllData(initial: AllData): AllData = {
    val updates: Seq[AllData => AllData] =
      Seq[AllData => AllData](
        allData => allData.modify(_.world.currentTick).using(_ + 1),
        allData => {
          val currentTick = allData.world.currentTick

          val updatedEggsAndLarvae: Seq[Either[Unit, AntBrood]] =
            allData.ants.eggsAndLarvae.map {
              case AntBrood.Egg(tick) if currentTick >= tick + AntBrood.defaultTicksToLarva =>
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
        },
        allData => {
          def countWorkers(task: AntTask): Long =
            allData.ants.tasks.find(_._1 == task).map(_._2 / u).getOrElse(0L)

          val sugarWorkers: Long = countWorkers(AntTask.SugarCollector)
          val buildWorkers: Long = countWorkers(AntTask.NestBuilder)

          val (updatedBuildQueue: Seq[BuildTask], finishedBuild: Option[BuildTask]) =
            allData.nestAttributes.buildQueue match {
              case buildTask +: otherTasks if buildWorkers > 0 =>
                val updatedBuildTask =
                  buildTask.useBuildPower(buildWorkers * Constants.defaultTaskBuildPowerTick)

                if (updatedBuildTask.isFinished)
                  (otherTasks, Some(updatedBuildTask))
                else
                  (updatedBuildTask +: otherTasks, None)
              case other =>
                (other, None)
            }

          val sugarBonusMultiplier: Double =
            if (allData.upgrades.improveSugarCollectorTask.unlocked) 1.15 else 1.0

          allData
            .giveResources(
              sugar =
                (sugarWorkers * Constants.defaultTaskCollectSugarTick * sugarBonusMultiplier).toLong,
            )
            .modify(_.nestAttributes.buildQueue)
            .setToIf(buildWorkers > 0)(updatedBuildQueue)
            .pipe { allData =>
              finishedBuild match {
                case None => allData
                case Some(BuildTask.NestUpgrade(_)) =>
                  allData
                    .modify(_.nestAttributes.nestLevel)
                    .using(_ + 1)
                    .modify(_.nestAttributes.maxWorkers)
                    .using(_ + Constants.NestUpgradeBonusMaxWorkers)
                case Some(BuildTask.QueenChamber(_)) =>
                  allData
                    .modify(_.nestAttributes.chambers.queenChamber.level)
                    .using(_ + 1)
                    .modify(_.nestAttributes.maxEggs)
                    .using(_ + Constants.QueenChamberBonusMaxEggs)
              }
            }
        },
        allData => {
          allData.giveResources(
            colonyPoints = allData.nestAttributes.nestLevel *
              allData.ants.workersLong *
              Constants.defaultNestLevelColonyPointsTick,
          )
        },
        allData => {
          val sugarUpkeep = allData.ants.workersLong * Constants.antsSugarUpkeepTick
          val sugarRemaining = allData.basicResources.sugar - sugarUpkeep

          allData
            .modify(_.basicResources.sugar)
            .setTo(Math.max(0, sugarRemaining))
            .pipe { allData =>
              if (sugarRemaining >= 0)
                allData
                  .modify(_.ants.sugarCumulativeDebt)
                  .using {
                    case debt if debt > 0 => Math.max(0, debt - sugarRemaining / 2)
                    case _                => 0
                  }
              else
                allData
                  .modify(_.ants.sugarCumulativeDebt)
                  .using { sugarCumulativeDebt => sugarCumulativeDebt - sugarRemaining }
                  .pipe { allData =>
                    if (
                      Constants.antDeathThisTick(
                        allData.world.saveSeed + allData.world.currentTick,
                        allData.ants.sugarCumulativeDebt,
                      )
                    )
                      allData
                        .addMessage("An ant died of starvation.")
                        .modify(_.ants.workers)
                        .using(_ - 1 * u)
                        .modify(_.ants)
                        .using { antsData =>
                          if (antsData.idleWorkersCount < 0)
                            antsData.remove1WorkerFromLastTask
                          else
                            antsData
                        }
                    else
                      allData
                  }
            }
        }
      )

    updates.foldLeft(initial) { case (allData, update) => update(allData) }
  }

}
