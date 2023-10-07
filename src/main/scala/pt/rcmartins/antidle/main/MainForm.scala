package pt.rcmartins.antidle.main

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.modifiers.EventListener
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.{HTMLDivElement, MouseEvent}
import pt.rcmartins.antidle.game.Constants._
import pt.rcmartins.antidle.model.{ActionCost, AntTask}
import pt.rcmartins.antidle.utils.Actions
import pt.rcmartins.antidle.utils.UIUtils.{prettyNumber, prettyNumberInt}
import pt.rcmartins.antidle.utils.Utils._

import scala.util.chaining.scalaUtilChainingOps

object MainForm {

  private val tooltipContent: Var[Signal[ReactiveHtmlElement[HTMLDivElement]]] = Var(Val(div()))
  private val tooltipTarget: Var[ReactiveHtmlElement[HTMLDivElement]] = Var(div())
  private val tooltipVisible: Var[Boolean] = Var(false)
  private val tooltipX: Var[Int] = Var(0)
  private val tooltipY: Var[Int] = Var(0)

  private val tooltip =
    div(
      className := "card p-1",
      position.absolute,
      top.px <-- tooltipY.signal.distinct.map(_ - 15),
      left.px <-- tooltipX.signal.distinct.map(_ + 50),
      display <-- tooltipVisible.signal.distinct.map(if (_) "block" else "none").distinct,
      child <-- tooltipContent.signal.flatten,
    )

  private def setTooltip(
      elem: ReactiveHtmlElement[HTMLDivElement],
      contentSignal: Signal[ReactiveHtmlElement[HTMLDivElement]]
  ): EventListener[MouseEvent, MouseEvent] =
    onMouseEnter --> { _ =>
      Var.set(
        tooltipVisible -> true,
        tooltipTarget -> elem,
        tooltipContent -> contentSignal,
      )
    }

  private val ResourcesWidth = 500
  private val NestWidth = 800
  private val MessagesWidth = 500

  def apply(): ReactiveHtmlElement[HTMLDivElement] = {
    initialize()
    div(
      className := "m-2",
      className := "d-flex justify-content-start",
      className := "fs-4",
      div(
        resourcesDiv,
      ),
      div(
        nestDiv,
        child.maybe <-- antTasksUnlockedSignal.map { if (_) Some(tasksDiv) else None },
      ),
      div(
        messagesDiv,
      ),
      onMouseMove --> (ev => {
        Var.set(
          tooltipX -> ev.clientX.toInt,
          tooltipY -> ev.clientY.toInt,
          tooltipVisible ->
            tooltipTarget
              .now()
              .ref
              .getBoundingClientRect()
              .pipe { rect =>
                ev.clientX >= rect.left &&
                  ev.clientX <= rect.right &&
                  ev.clientY >= rect.top &&
                  ev.clientY <= rect.bottom
              }
        )
      }),
      tooltip
    )
  }

  def messagesDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "m-1",
      className := "card",
      className := "fs-4",
      width.px := MessagesWidth,
      maxWidth.px := MessagesWidth,
      div(
        className := "card-header",
        "Messages",
      ),
      div(
        className := "card-body",
        div(
          className := "d-grid gap-2",
          child.maybe <-- lastMessage.map(_.map(text => span(text)))
        )
      )
    )

  def resourcesDiv: Modifier[ReactiveHtmlElement[HTMLDivElement]] =
    div(
      className := "m-1",
      className := "card",
      width.px := ResourcesWidth,
      maxWidth.px := ResourcesWidth,
      div(
        className := "card-header",
        "Resources",
      ),
      div(
        className := "card-body",
        child.maybe <-- createResourceDiv("Sugars", sugarsSignal, maxSugars, Val(true)),
//        child.maybe <-- createResourceDiv("Proteins", proteinsSignal, Val(0),Val(false)),
//        child.maybe <-- createResourceDiv("Water", waterSignal, Val(0),Val(false)),
        child.maybe <-- createResourceDiv("Colony Points", colonyPointsSignal, Val(0), Val(false)),
//        child.maybe <-- createResourceDiv("DNA", DNASignal, Val(0),Val(false)),
        nbsp,
        child.maybe <-- createResourceDiv(
          "Eggs",
          eggsCountSignal,
          maxEggs,
          unlocksSignal.map(_.layedFirstEgg)
        ),
        child.maybe <-- createResourceDiv(
          "Workers",
          workersSignal,
          maxWorkers,
          unlocksSignal.map(_.bornFirstWorker)
        ),
      )
    )

  def createResourceDiv(
      name: String,
      valueSignal: Signal[Long],
      maxValueSignal: Signal[Long],
      showSignal: Signal[Boolean],
  ): Signal[Option[ReactiveHtmlElement[HTMLDivElement]]] =
    valueSignal.combineWith(showSignal).map {
      case (_, false) =>
        None
      case (value, true) =>
        Some(
          div(
            className := "row",
            div(
              className := "col-4 text-nowrap",
              name,
            ),
            div(
              className := "col-3 text-end text-nowrap",
              prettyNumber(value),
            ),
            div(
              className := "col-2 text-start text-body-tertiary text-nowrap ps-0",
              child <--
                maxValueSignal.map { maxValue =>
                  if (maxValue == 0)
                    nbsp
                  else
                    span("/", prettyNumber(maxValue))
                },
            ),
            div(
              className := "col-3 text-end text-nowrap",
              nbsp,
            )
          )
        )
    }

  def nestDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "m-1",
      className := "card",
      width.px := NestWidth,
      maxWidth.px := NestWidth,
      div(
        className := "card-header",
        "Nest",
      ),
      div(
        className := "card-body",
        div(
          className := "row",
          div(
            className := "col-6 p-2 d-grid",
            button(
              className := "btn btn-primary",
              className := "fs-4",
              `type` := "button",
              "Gather Sugar",
              onClick --> { _ => giveResources(sugars = PlayerGatherSugarClickAmount) }
            ),
          ),
          child.maybe <-- unlocksSignal.map(_.canLayEggs).map {
            case false =>
              None
            case true =>
              Some(
                actionButton(
                  id = "action-lay-egg",
                  name = "Lay Egg",
                  Actions.layEggActionEnabled,
                  Actions.layEggActionCost,
                  () => Actions.layEggAction(),
                )
              )
          },
        )
      )
    )

  private def actionButton(
      id: String,
      name: String,
      enabledSignal: Signal[Boolean],
      costSignal: Signal[ActionCost],
      onClickAction: () => Unit,
  ): ReactiveHtmlElement[HTMLDivElement] =
    div(
      idAttr := id,
      className := "col-6 p-2 d-grid",
      button(
        className := "btn btn-primary",
        className := "fs-4",
        `type` := "button",
        name,
        disabled <-- enabledSignal.map(!_),
        onClick --> { _ => onClickAction() },
      ),
    ).amendThis(elem =>
      setTooltip(
        elem,
        costSignal.map { cost =>
          div(s"Cost: ", prettyNumber(cost.sugars), " sugars")
        }
      )
    )

  def tasksDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "m-1",
      className := "card",
      width.px := NestWidth,
      maxWidth.px := NestWidth,
      div(
        className := "card-header",
        "Tasks",
      ),
      div(
        className := "card-body",
        div(
          className := "d-grid gap-2",
          span(
            "Idle Ants: ",
            b(child <-- idleWorkersCountSignal.map(prettyNumber)),
          ),
          children <-- unlockedAntTasks.map(_.map(taskDiv)),
        )
      )
    )

  def taskDiv(antTask: AntTask): ReactiveHtmlElement[HTMLDivElement] = {
    val amountSignal =
      antsSignal.map(_.tasks.find(_._1 == antTask).map(_._2).getOrElse(0L)).distinct
    val id = s"task-${antTask.toString.toLowerCase}"

    val tooltipSignal = antTask match {
      case AntTask.Nursary =>
        Val(div("???"))
      case AntTask.SugarCollector =>
        Val(div(s"+", prettyNumber(DefaultTaskCollectSugarSecond), " sugar per second / ant"))
      case AntTask.WaterCollector =>
        Val(div("???"))
    }

    div(
      idAttr := id,
      className := "row",
      span(
        className := "col-5",
        antTask match {
          case AntTask.Nursary        => "Nursary"
          case AntTask.SugarCollector => "Sugar Collector"
          case AntTask.WaterCollector => "Water Collector"
        },
        nbsp,
        "(",
        child <-- amountSignal.map(prettyNumberInt),
        ")",
      ),
      div(
        className := "col-2",
        button(
          className := "btn btn-primary",
          className := "m-1",
          cursor.pointer,
          i(
            className := "fa-solid fa-minus",
          ),
          disabled <-- amountSignal.map(_ == 0),
          onClick --> { _ => Actions.reduceAntTask(antTask) }
        ),
        button(
          className := "btn btn-primary",
          className := "m-1",
          cursor.pointer,
          i(
            className := "fa-solid fa-plus",
          ),
          disabled <-- idleWorkersCountSignal.map(_ == 0),
          onClick --> { _ => Actions.incrementAntTask(antTask) }
        ),
      ),
      div(className := "col-5"),
    ).amendThis(elem => setTooltip(elem, tooltipSignal))
  }

}
