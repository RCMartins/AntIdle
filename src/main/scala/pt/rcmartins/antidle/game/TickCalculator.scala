package pt.rcmartins.antidle.game

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.{HTMLDivElement, HTMLElement}
import pt.rcmartins.antidle.game.Constants._
import pt.rcmartins.antidle.game.TickCalculator.BonusOrigin.{PercentOrigin, ResourceOrigin}
import pt.rcmartins.antidle.game.UIUtils.setTooltip
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.model.AntTask
import pt.rcmartins.antidle.model.BasicResources.BasicResource
import pt.rcmartins.antidle.model.UpgradesData.UpgradeType._

import scala.annotation.tailrec

object TickCalculator {

  sealed trait BonusOrigin {

    def name: String

  }

  object BonusOrigin {

    case class ResourceOrigin(
        name: String,
        tickResourceU: Long,
    ) extends BonusOrigin {

      def resourceUSec: Long = tickResourceU * TicksPerSecond

    }

    case class PercentOrigin(
        name: String,
        percent: Double,
    ) extends BonusOrigin

  }

  private def antTaskCountSignal(antTask: AntTask): Signal[Long] =
    antsSignal.map(_.tasks.find(_._1 == antTask).map(_._2 / u).getOrElse(0L)).distinct

  val sugarTickGain: Signal[List[BonusOrigin]] =
    antTaskCountSignal(AntTask.SugarCollector)
      .combineWith(
        upgradesSignal,
        antsSignal.map(_.upkeepWorkersCount).distinct,
      )
      .map {
        case (
              sugarCollectorAmount,
              upgrades,
              upkeepWorkersCount
            ) =>
          List(
            Some(
              ResourceOrigin(
                AntTask.SugarCollector.name,
                sugarCollectorAmount * defaultTaskCollectSugarTick,
              )
            ),
            Some(PercentOrigin(ImproveSugarCollectorTask1.title, SugarImprovementBonusPerc))
              .filter(_ => upgrades(ImproveSugarCollectorTask1).unlocked),
            Some(PercentOrigin(ImproveSugarCollectorTask2.title, SugarImprovementBonusPerc))
              .filter(_ => upgrades(ImproveSugarCollectorTask2).unlocked),
            Some(PercentOrigin(ImproveSugarCollectorTask3.title, SugarImprovementBonusPerc))
              .filter(_ => upgrades(ImproveSugarCollectorTask3).unlocked),
            Some(
              ResourceOrigin(
                "Ants Demand",
                -upkeepWorkersCount * antsSugarUpkeepTick,
              )
            ),
          ).flatten
      }

  val tunnelingSpaceTickGain: Signal[List[BonusOrigin]] =
    antTaskCountSignal(AntTask.Tunneler)
      .map { tunnelersAmount =>
        List(
          Some(
            ResourceOrigin(
              AntTask.Tunneler.name,
              tunnelersAmount * defaultTaskTunnelingSpaceTick,
            )
          ),
        ).flatten
      }

  val colonyPointsTickGain: Signal[List[BonusOrigin]] =
    antsSignal
      .map(_.workersCount)
      .distinct
      .combineWith(chambersSignal.map(_.nestChamber.level).distinct)
      .map { case (workersCount, nestChamberLevel) =>
        List(
          Some(
            ResourceOrigin(
              "Workers",
              nestChamberLevel * workersCount * defaultNestLevelColonyPointsTick,
            )
          ),
        ).flatten
      }

  // TODO show <diff color> when full ? -> orange maybe?
  def prettyResourceEstDiv(
      listSignal: Signal[List[BonusOrigin]],
      resource: Signal[BasicResource],
  ): ReactiveHtmlElement[HTMLDivElement] = {
    @tailrec
    def calcLoop(list: List[BonusOrigin], sum: Long): Long =
      list match {
        case Nil =>
          sum
        case head :: tail =>
          head match {
            case ResourceOrigin(_, resourceTickU) =>
              calcLoop(tail, sum + resourceTickU)
            case PercentOrigin(_, percent) =>
              calcLoop(tail, (sum * percent).toLong)
          }
      }

    val sumUSignal: Signal[Long] = listSignal.map(calcLoop(_, 0L))

    div(
      child <--
        sumUSignal.map { sumU =>
          val useMinutes: Boolean = Math.abs(sumU) < 20
          val minutesMultiplier: Int = if (useMinutes) 60 else 1

          span(
            if (sumU > 0) "+" else if (sumU < 0) "-" else "",
            UINumbersUtils.prettyNumberStr(minutesMultiplier * sumU * TicksPerSecond),
            if (useMinutes) "/m" else "/s",
          )
        }
    ).amendThis(elem => setTooltip(elem, prettyResourceTooltip(listSignal, sumUSignal, resource)))
  }

  // TODO some colors= green for positive, red for negative, orange for 0 ?
  private def prettyResourceTooltip(
      listSignal: Signal[List[BonusOrigin]],
      sumSignal: Signal[Long],
      resource: Signal[BasicResource],
  ): ReactiveHtmlElement[HTMLDivElement] =
    div(
      children <-- listSignal.map {
        _.map { bonusOrigin =>
          div(
            className := "d-flex justify-content-between",
            span(
              className := "pe-3",
              s"${bonusOrigin.name}:",
            ),
            span(
              // TODO deal this small numbers in minutes
              bonusOrigin match {
                case resource @ ResourceOrigin(_, _) if resource.resourceUSec > 0 =>
                  s"+${UINumbersUtils.prettyNumberStr(resource.resourceUSec)}/sec"
                case resource @ ResourceOrigin(_, _) if resource.resourceUSec < 0 =>
                  s"${UINumbersUtils.prettyNumberStr(resource.resourceUSec)}/sec"
                case ResourceOrigin(_, _) =>
                  s"0/sec"
                case PercentOrigin(_, percent) if percent > 0 =>
                  f"+${percent - 1.0}%.2f%%"
                case PercentOrigin(_, percent) if percent < 0 =>
                  f"${percent - 1.0}%.2f%%"
                case PercentOrigin(_, _) =>
                  "0.00%"
              }
            )
          )
        }
      },
      hr(),
      child <-- resource.combineWith(sumSignal).map { case (resource, sumU) =>
        div(
          className := "d-flex justify-content-between",
          if (sumU >= 0) "To Full: " else "To Empty: ",
          span(
            // TODO deal this small numbers in minutes
            if (sumU > 0) {
              val timeToFull: Long = resource.freeSpace / sumU
              if (timeToFull == 0)
                "0s"
              else
                s"${UINumbersUtils.prettyTimeFromTicks(timeToFull)}"
            } else if (sumU < 0) {
              val timeToEmpty: Long = resource.amount / -sumU
              if (timeToEmpty == 0)
                "0s"
              else
                s"${UINumbersUtils.prettyTimeFromTicks(timeToEmpty)}"
            } else {
              "Never"
            }
          )
        )
      },
    )

}
