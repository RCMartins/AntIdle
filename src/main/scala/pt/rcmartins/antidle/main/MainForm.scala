package pt.rcmartins.antidle.main

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.nodes.ReactiveHtmlElement.Base
import org.scalajs.dom.{HTMLButtonElement, HTMLDivElement}
import pt.rcmartins.antidle.model.AntTask
import pt.rcmartins.antidle.utils.Actions
import pt.rcmartins.antidle.utils.Utils._

import scala.scalajs.js

object MainForm {

  private def createTooltip(title: String): Seq[Modifier[Base]] =
    Seq(
      htmlAttr("data-bs-toggle", StringAsIsCodec) := "tooltip",
      htmlAttr("data-bs-placement", StringAsIsCodec) := "bottom",
      htmlAttr("data-bs-title", StringAsIsCodec) := title,
      onMountCallback { _ =>
        js.Dynamic.global.initializeTooltips()
      }
    )

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
//        child.maybe <-- createResourceDiv(
//          "Larvae",
//          antsSignal.map(_.eggsAndLarvae.size),
//          unlocksSignal.map(_.larvaeUnlocked)
//        ),
//        child.maybe <-- createResourceDiv(
//          "Pupae",
//          antsSignal.map(_.eggsAndLarvae.size),
//          unlocksSignal.map(_.pupaeUnlocked)
//        ),
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
              className := "col-3 text-nowrap",
              name,
            ),
            div(
              className := "col-3 text-end text-nowrap",
              value,
            ),
            div(
              className := "col-3 text-start text-body-tertiary text-nowrap",
              child <--
                maxValueSignal.map { maxValue =>
                  if (maxValue == 0)
                    nbsp
                  else
                    span(
                      s"/$maxValue",
                    )
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
              onClick --> { _ => giveResources(sugars = 1) }
            ),
          ),
          child.maybe <-- unlocksSignal.map(_.canLayEggs).map {
            case false =>
              None
            case true =>
              Some(
                actionButton(
                  "Lay Egg",
                  Actions.layEggActionEnabled,
                  () => Actions.layEggAction(),
                )
              )
          },
        )
      )
    )

  private def actionButton(
      name: String,
      enabledSignal: Signal[Boolean],
      onClickAction: () => Unit,
  ): ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "col-6 p-2 d-grid",
      button(
        className := "btn btn-primary",
        className := "fs-4",
        `type` := "button",
        name,
        disabled <-- enabledSignal.map(!_),
        onClick --> { _ => onClickAction() },
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
          children <-- unlockedAntTasks.map(_.map(taskDiv)),
        )
      )
    )

  def taskDiv(antTask: AntTask): ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "d-flex flex-fill justify-content-between",
      span(
        antTask match {
          case AntTask.Nursary        => "Nursary"
          case AntTask.SugarCollector => "Sugar Collector"
          case AntTask.WaterCollector => "Water Collector"
        }
      ),
      span(
        child.text <-- antsSignal.map(_.tasks.find(_._1 == antTask).map(_._2).getOrElse(0L)),
        nbsp,
        "/",
        nbsp,
        child.text <-- idleWorkersCountSignal,
      ),
      div(
        button(
          className := "btn btn-primary",
          className := "m-1",
          `type` := "button",
          "-",
          onClick --> { _ => () }
        ),
        button(
          className := "btn btn-primary",
          className := "m-1",
          `type` := "button",
          "+",
          onClick --> { _ => () }
        ),
      )
    )

}
