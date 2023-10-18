package pt.rcmartins.antidle.main

import com.raquo.airstream.ownership.OneTimeOwner
import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.modifiers.EventListener
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.{window, HTMLDivElement, HTMLSpanElement, MouseEvent}
import pt.rcmartins.antidle.game.Constants._
import pt.rcmartins.antidle.game.UIUtils._
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.game.{Actions, Constants, SaveLoad}
import pt.rcmartins.antidle.model._

import scala.util.chaining.scalaUtilChainingOps

object MainForm {

  private val tooltipContent: Var[Signal[ReactiveHtmlElement[HTMLDivElement]]] = Var(Val(div()))
  private val tooltipTarget: Var[ReactiveHtmlElement[HTMLDivElement]] = Var(div())
  private val tooltipVisible: Var[Boolean] = Var(false)
  private val tooltipX: Var[Int] = Var(0)
  private val tooltipY: Var[Int] = Var(0)

  private val tooltip: ReactiveHtmlElement[HTMLDivElement] =
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

  private val windowWidthVar: Var[Long] = Var(1000)
  private val windowWidthLimit: Long =
    ResourcesWidth + NestWidth + MessagesWidth

  def apply(): ReactiveHtmlElement[HTMLDivElement] = {
    val owner = new OneTimeOwner(() => println("MainForm owner is being called after dispose"))

    dom.window.addEventListener(
      "resize",
      (_: dom.Event) => {
        Var.set(
          windowWidthVar -> window.innerWidth.toLong
        )
      }
    )

    initialize()

    div(
      className := "m-2",
      className := "d-flex justify-content-start",
      className := "fs-4",
      className.toggle("flex-column") <-- windowWidthVar.signal.map(_ < windowWidthLimit),
      div(
        resourcesDiv,
      ),
      div(
        className := "card m-1",
        width.px := NestWidth,
        maxWidth.px := NestWidth,
        div(
          className := "card-header",
          ul(
            className := "nav nav-tabs card-tabs",
            role := "tablist",
            li(
              className := "nav-item",
              a(
                className := "nav-link active",
                idAttr := "nest-tab",
                dataAttr("bs-toggle") := "tab",
                href := "#nestContent",
                role := "tab",
                "Nest"
              )
            ),
            child.maybe <-- ifUnlockedOpt(antTasksUnlockedSignal)(
              li(
                className := "nav-item",
                a(
                  className := "nav-link",
                  idAttr := "profile-tab",
                  dataAttr("bs-toggle") := "tab",
                  href := "#tasksContent",
                  role := "tab",
                  "Tasks",
                  child <-- idleWorkersCountSignal.map {
                    case 0 => span()
                    case n => span(s" (", prettyNumberInt(n), ")")
                  },
                )
              )
            ),
            child.maybe <-- ifUnlockedOpt(upgradesTabUnlockedSignal)(
              li(
                className := "nav-item",
                a(
                  className := "nav-link",
                  idAttr := "profile-tab",
                  dataAttr("bs-toggle") := "tab",
                  href := "#upgradesContent",
                  role := "tab",
                  "Upgrades",
                )
              )
            ),
          )
        ),
        div(
          className := "card-body",
          div(
            className := "tab-content",
            div(
              className := "tab-pane show active",
              idAttr := "nestContent",
              role := "tabpanel",
              nestDiv(owner),
            ),
            child.maybe <-- ifUnlockedOpt(antTasksUnlockedSignal)(
              div(
                className := "tab-pane",
                idAttr := "tasksContent",
                role := "tabpanel",
                tasksDiv,
              )
            ),
            child.maybe <-- ifUnlockedOpt(antTasksUnlockedSignal)(
              div(
                className := "tab-pane",
                idAttr := "upgradesContent",
                role := "tabpanel",
                upgradesDiv(owner),
              )
            ),
          )
        )
      ),
      div(
        saveLoadDiv,
        child.maybe <-- ifUnlockedOpt(buildQueueUnlockedSignal)(buildQueueDiv),
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
      tooltip,
      onMountCallback { _ =>
        windowWidthVar.set(window.innerWidth.toLong)
      },
    )
  }

  private def saveLoadDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "card m-1",
      div(
        className := "row m-0",
        button(
          className := "btn btn-primary col-5 m-1",
          className := "fs-4",
          `type` := "button",
          "Save",
          onClick --> { _ =>
            actionUpdater.writer.onNext(allData => allData.tap(SaveLoad.saveToLocalStorage))
          }
        ),
        button(
          className := "btn btn-primary col-5 m-1",
          className := "fs-4",
          `type` := "button",
          "Load",
          onClick --> { _ =>
            SaveLoad
              .loadFromLocalStorage()
              .foreach(loadedAllData => actionUpdater.writer.onNext(_ => loadedAllData))
          }
        ),
      )
    )

  private def buildQueueDiv: ReactiveHtmlElement[HTMLDivElement] = {
    def buildTaskName(buildTask: BuildTask): ReactiveHtmlElement[HTMLSpanElement] = {
      val name: String =
        buildTask match {
          case _: BuildTask.NestUpgrade  => Constants.NestUpgradeName
          case _: BuildTask.QueenChamber => Constants.QueenChamberName
        }
      span(name, " [", prettyNumber1d(buildTask.buildPowerRequired), "]")
    }

    div(
      className := "m-1",
      className := "card",
      className := "fs-4",
      width.px := MessagesWidth,
      maxWidth.px := MessagesWidth,
      div(
        className := "card-header",
        div(
          span(
            "Build Queue (",
            child.text <-- buildQueueSignal.map(_.size),
            "/",
            child.text <-- maxBuildQueueSignal,
            ")"
          ),
        )
      ),
      div(
        className := "card-body",
        div(
          className := "d-grid gap-2",
          child <-- buildQueueSignal.map {
            case Seq() => span(nbsp)
            case seq   => span(seq.map(buildTask => buildTaskName(buildTask)))
          }
        )
      )
    )
  }

  private def messagesDiv: ReactiveHtmlElement[HTMLDivElement] =
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
        className := "fs-5",
        div(
          className := "d-grid gap-2",
          child.maybe <-- messagesSignal.signal.map(_.headOption.map(text => span(text))),
          children <--
            messagesSignal.map {
              _.drop(1).map(text => div(opacity := 0.5, text))
            }
        )
      )
    )

  private def resourcesDiv: Modifier[ReactiveHtmlElement[HTMLDivElement]] =
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
        child.maybe <-- createResourceDiv("Sugar", sugarSignal, maxSugar, Val(true)),
        child.maybe <-- createResourceDiv(
          "Colony Points",
          colonyPointsSignal,
          Val(0),
          unlocksSignal.map(_.resources.showColonyPoints).distinct,
        ),
        nbsp,
        child.maybe <-- createResourceDiv(
          "Eggs",
          eggsCountSignal,
          maxEggs,
          unlocksSignal.map(_.resources.showEggs)
        ),
        child.maybe <-- createResourceDiv(
          "Workers",
          workersSignal,
          maxWorkers,
          unlocksSignal.map(_.resources.showWorkers)
        ),
      )
    )

  private def createResourceDiv(
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

  private def nestDiv(owner: Owner): ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "row",
      actionButton(
        name = "Gather Sugar",
        Val(None),
        Val(true),
        Val(None),
        Val(ActionCost.empty),
        Val(ActionBonus(sugar = 1 * u)),
        () => giveResources(sugar = PlayerGatherSugarClickAmount),
      ),
      child.maybe <-- ifUnlockedOpt(_.actions.canLayEggs) {
        actionButton(
          name = "Lay Egg",
          Val(None),
          Actions.layEggActionEnabled,
          Val(Some(div("Ants require a constant sugar supply to stay alive."))),
          Actions.layEggActionCost,
          Val(ActionBonus(eggs = 1 * u)),
          () => Actions.layEggAction(),
        )
      },
      child.maybe <-- ifUnlockedOpt(_.actions.canBuildNestUpgrade) {
        actionButton(
          name = Constants.NestUpgradeName,
          nestSignal.map(_.nestLevel).distinct.map(Some(_)),
          Actions.nestUpgradeEnabled,
          Val(None),
          Actions.nestUpgradeCost,
          Val(
            ActionBonus(
              colonyPointsEachWorker = Constants.defaultNestLevelColonyPointsSecond,
              maxWorkers = 2 * u,
            )
          ),
          () =>
            useSignalValue[ActionCost](
              owner,
              Actions.nestUpgradeCost,
              actionCost => Actions.addBuildTask(BuildTask.NestUpgrade(actionCost.buildPower))
            ),
        )
      },
      child.maybe <-- ifUnlockedOpt(_.actions.canBuildQueenChamber) {
        actionButton(
          name = Constants.QueenChamberName,
          nestSignal.map(_.chambers.queenChamber.level).map(Some(_)),
          Actions.queenChamberBuyEnabled,
          Val(None),
          Actions.queenChamberCost,
          Val(
            ActionBonus(
              maxEggs = 2 * u,
            )
          ),
          () =>
            useSignalValue[ActionCost](
              owner,
              Actions.queenChamberCost,
              actionCost => Actions.addBuildTask(BuildTask.QueenChamber(actionCost.buildPower))
            ),
        )
      },
    )

  private def actionButton(
      name: String,
      levelSignal: Signal[Option[Int]],
      enabledSignal: Signal[Boolean],
      descriptionSignal: Signal[Option[ReactiveHtmlElement[HTMLDivElement]]],
      costSignal: Signal[ActionCost],
      bonusSignal: Signal[ActionBonus],
      onClickAction: () => Unit,
  ): ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "col-6 p-2 d-grid",
      button(
        className := "btn btn-primary position-relative",
        className := "fs-4",
        `type` := "button",
        name,
        disabled <-- enabledSignal.map(!_),
        onClick --> { _ => onClickAction() },
        child.maybe <--
          levelSignal.map {
            _.filter(_ > 0).map { level =>
              span(
                className := "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-secondary",
                level,
              )
            }
          },
      ),
    ).amendThis(elem =>
      setTooltip(
        elem,
        Val(
          div(
            className := "p-1",
            child.maybe <-- descriptionSignal.map {
              _.map { descriptionDiv =>
                div(
                  className := "text-center",
                  maxWidth.px := 350,
                  descriptionDiv,
                  hr(),
                )
              }
            },
            child <-- costSignal.map { cost =>
              div(
                className := "d-flex flex-column justify-content-center text-center",
                ifGreater0(cost.sugar)(prettyNumberSimple("", _, " sugar")),
                ifGreater0(cost.buildPower)(prettyNumberSimple("", _, " build power")),
                ifGreater0(cost.colonyPoints)(prettyNumberSimple("", _, " colony points")),
              )
            },
            child.maybe <-- costSignal.combineWith(bonusSignal).map { case (cost, bonus) =>
              if (cost == ActionCost.empty || bonus == ActionBonus.empty)
                None
              else
                Some(hr())
            },
            child <-- bonusSignal.map { bonus =>
              div(
                className := "d-flex flex-column justify-content-center text-center",
                ifGreater0(bonus.sugar)(prettyNumberSimple("+", _, " sugar")),
                ifGreater0(bonus.eggs)(prettyNumberSimple("+", _, " eggs")),
                ifGreater0(bonus.colonyPointsEachWorker)(
                  prettyNumberSimple("+", _, " colony points/worker/s")
                ),
                ifGreater0(bonus.maxSugar)(prettyNumberSimple("+", _, " max sugar")),
                ifGreater0(bonus.maxEggs)(prettyNumberSimple("+", _, " max eggs")),
                ifGreater0(bonus.maxWorkers)(prettyNumberSimple("+", _, " max workers")),
              )
            },
          )
        ),
      )
    )

  private def tasksDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "d-grid gap-2",
      span(
        "Idle Ants: ",
        b(child <-- idleWorkersCountSignal.map(prettyNumber)),
      ),
      children <-- unlockedAntTasks.map(_.map(taskDiv)),
    )

  private def taskDiv(antTask: AntTask): ReactiveHtmlElement[HTMLDivElement] = {
    val amountSignal =
      antsSignal.map(_.tasks.find(_._1 == antTask).map(_._2).getOrElse(0L)).distinct
    val id = s"task-${antTask.toString.toLowerCase}"

    val tooltipSignal: Val[ReactiveHtmlElement[HTMLDivElement]] = antTask match {
      case AntTask.SugarCollector =>
        Val(div(prettyNumberSimple("+", defaultTaskCollectSugarSecond, " sugar per second / ant")))
      case AntTask.WaterCollector =>
        Val(div("???"))
      case AntTask.Nursery =>
        Val(div("???"))
      case AntTask.NestBuilder =>
        Val(
          div(prettyNumberSimple("+", defaultTaskBuildPowerSecond, " build power per second / ant"))
        )
    }

    div(
      idAttr := id,
      className := "row",
      span(
        className := "col-6",
        antTask match {
          case AntTask.SugarCollector => "Sugar Collector"
          case AntTask.WaterCollector => "Water Collector"
          case AntTask.NestBuilder    => "Nest Builder"
          case AntTask.Nursery        => "Nursery"
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
      div(className := "col-4"),
    ).amendThis(elem => setTooltip(elem, tooltipSignal))
  }

  private def upgradesDiv(owner: Owner): ReactiveHtmlElement[HTMLDivElement] = {
    def createUpgrade(
        name: String,
        upgradeFunc: UpgradesData => UpgradeData,
        description: Signal[Option[ReactiveHtmlElement[HTMLDivElement]]],
        unlock: ActionCost => Unit,
    ): Signal[Option[ReactiveHtmlElement[HTMLDivElement]]] = {
      val costSignal: Signal[ActionCost] = upgradesSignal.map(upgradeFunc(_).cost)
      ifUpgradeReadyToBuyOpt(upgradeFunc) {
        actionButton(
          name = name,
          Val(None),
          hasResourcesSignal(costSignal),
          description,
          costSignal,
          Val(ActionBonus.empty),
          () =>
            useSignalValue[ActionCost](
              owner,
              costSignal,
              actionCost => unlock(actionCost)
            ),
        )
      }
    }

    div(
      className := "row",
      child.maybe <-- createUpgrade(
        name = "Unlock Queen chamber",
        _.queensChamber,
        description = Val(Some(div("Improves the queen's chamber abilities."))),
        unlock = Actions.unlockQueenChamberUpgrade,
      ),
      child.maybe <-- createUpgrade(
        name = "Foraging Techniques",
        _.improveSugarCollectorTask,
        description = Val(Some(div("Improves the ants sugar collecting amount by 15%."))),
        unlock = Actions.unlockImproveSugarCollectorTask,
      ),
    )
  }

}
