package pt.rcmartins.antidle.main

import com.raquo.airstream.ownership.OneTimeOwner
import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.modifiers.EventListener
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.softwaremill.quicklens.ModifyPimp
import org.querki.jquery
import org.scalajs.dom
import org.scalajs.dom._
import pt.rcmartins.antidle.game.Constants._
import pt.rcmartins.antidle.game.TickCalculator.prettyResourceEstDiv
import pt.rcmartins.antidle.game.UINumbersUtils._
import pt.rcmartins.antidle.game.UIUtils._
import pt.rcmartins.antidle.game.Utils._
import pt.rcmartins.antidle.game._
import pt.rcmartins.antidle.game.saves.SaveLoad
import pt.rcmartins.antidle.model.BasicResources.BasicResource
import pt.rcmartins.antidle.model.Chamber.ChamberType
import pt.rcmartins.antidle.model.Unlocks.ActionUnlocks
import pt.rcmartins.antidle.model.UpgradesData.UpgradeType
import pt.rcmartins.antidle.model._

import scala.scalajs.concurrent.JSExecutionContext.queue
import scala.scalajs.js
import scala.util.chaining.scalaUtilChainingOps

object MainForm {

  val currentGlobalAlert: Var[Option[String]] = Var(None)

  private val tooltipX: Var[Int] = Var(0)
  private val tooltipY: Var[Int] = Var(0)

  private val tooltip: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "card p-1",
      position.absolute,
      top.px <-- tooltipY.signal.distinct.map(_ - 15),
      left.px <-- tooltipX.signal.distinct.map(_ + 50),
      display <-- tooltipVisible.signal.distinct.map(if (_) "block" else "none").distinct,
      child <-- tooltipContent.signal,
    )

  private val ctrlPressedVar = Var(false)
  private val shiftPressedVar = Var(false)

  private val modifiersMultiplierSignal: Signal[Int] =
    ctrlPressedVar.signal
      .combineWith(shiftPressedVar.signal)
      .map { case (ctrl, shift) =>
        getMouseEventMultiplier(ctrl, shift)
      }

  private val ResourcesWidth = 500
  private val NestWidth = 800
  private val MessagesWidth = 575

  private val windowWidthVar: Var[Long] = Var(1000)
  private val windowWidthLimit: Long =
    ResourcesWidth + NestWidth + MessagesWidth

  def apply(): ReactiveHtmlElement[HTMLDivElement] = {
    implicit val owner: Owner = new OneTimeOwner(() =>
      println("MainForm owner is being called after dispose")
    )

    dom.window.addEventListener(
      "resize",
      (_: dom.Event) => {
        Var.set(
          windowWidthVar -> window.innerWidth.toLong
        )
      }
    )

    initialize()

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      if (e.ctrlKey) ctrlPressedVar.set(true)
      if (e.shiftKey) shiftPressedVar.set(true)
    }
    dom.document.onkeyup = (e: dom.KeyboardEvent) => {
      if (!e.ctrlKey) ctrlPressedVar.set(false)
      if (!e.shiftKey) shiftPressedVar.set(false)
    }

    div(
      topBarDiv,
      div(
        className := "m-2",
        className := "d-flex justify-content-start",
        className := "fs-4",
        className("flex-column") <-- windowWidthVar.signal.map(_ < windowWidthLimit),
        div(
          resourcesDiv,
        ),
        mainTabsDiv,
        div(
          child.maybe <-- ifUnlockedOpt(buildQueueUnlockedSignal)(buildQueueDiv),
          messagesDiv,
        ),
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
      Modals.importModal(),
      Modals.newGameModal(),
      UIUtils.createShowGlobalAlertsDiv(),
    )
  }

  private def topBarDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "flex-column",
      div(
        className := "m-2",
        div(
          className := "w-100",
          height.px := 26,
          className := "d-flex justify-content-between align-items-center",
          div(
            "Ant Idle"
          ),
          div(
            className := "d-flex justify-content-between align-items-center",
            timeDiv,
            div(
              width.px := 75,
              nbsp,
            ),
            catchUpDiv,
          ),
          div(
            nbsp
          ),
        ),
      ),
      hr(
        className := "w-100",
        className := "m-0",
      ),
    )

  private def timeDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      child <--
        currentTickSignal.map { currentTick =>
          val years = currentTick / TicksPerYear
          val season: Int = ((currentTick - TicksPerYear * years) / TicksPerSeason).toInt
          val day = (currentTick % TicksPerSeason) / TicksPerDay

          span("Year ", years, " - ", Constants.seasonStr(season), ", day ", day + 1)
        }
    )

  private def catchUpDiv: ReactiveHtmlElement[HTMLDivElement] = {
    val isBehind: Signal[Boolean] =
      currentTickSignal
        .combineWith(targetTickSignal)
        .map { case (currentTick, targetTick) =>
          targetTick - currentTick > 0
        }
        .distinct

    div(
      child <--
        isBehind.map {
          case false =>
            span()
          case true =>
            span(
              "Catch up: [",
              child.text <--
                currentTickSignal.combineWith(targetTickSignal).map {
                  case (currentTick, targetTick) => prettyTimeFromTicks(targetTick - currentTick)
                },
              "] ",
              button(
                className := "btn btn-sm py-0",
                className("btn-success active") <-- Utils.useCatchupTicksVar,
                className("btn-secondary") <-- Utils.useCatchupTicksVar.signal.map(!_),
                `type` := "button",
                child.text <-- Utils.useCatchupTicksVar.signal.map(if (_) "On" else "Off"),
                onClick --> { _ =>
                  Utils.useCatchupTicksVar.update(!_)
                }
              )
            )
        }
    )
  }

  private def mainTabsDiv(implicit owner: Owner): ReactiveHtmlElement[HTMLDivElement] =
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
                idAttr := "tasks-tab",
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
                idAttr := "upgrades-tab",
                dataAttr("bs-toggle") := "tab",
                href := "#upgradesContent",
                role := "tab",
                "Upgrades",
              )
            )
          ),
          child.maybe <-- ifUnlockedOpt(exploreTabUnlockedSignal)(
            li(
              className := "nav-item",
              a(
                className := "nav-link",
                idAttr := "upgrades-tab",
                dataAttr("bs-toggle") := "tab",
                href := "#exploreContent",
                role := "tab",
                "Explore",
              )
            )
          ),
          li(
            className := "nav-item",
            a(
              className := "nav-link",
              idAttr := "settings-tab",
              dataAttr("bs-toggle") := "tab",
              href := "#settingsContent",
              role := "tab",
              "Settings"
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
            nestDiv,
          ),
          child.maybe <-- ifUnlockedOpt(antTasksUnlockedSignal)(
            div(
              className := "tab-pane",
              idAttr := "tasksContent",
              role := "tabpanel",
              tasksDiv,
            )
          ),
          child.maybe <-- ifUnlockedOpt(upgradesTabUnlockedSignal)(
            div(
              className := "tab-pane",
              idAttr := "upgradesContent",
              role := "tabpanel",
              upgradesDiv,
            )
          ),
          child.maybe <-- ifUnlockedOpt(exploreTabUnlockedSignal)(
            div(
              className := "tab-pane",
              idAttr := "exploreContent",
              role := "tabpanel",
              exploreDiv,
            )
          ),
          div(
            className := "tab-pane",
            idAttr := "settingsContent",
            role := "tabpanel",
            settingsDiv,
          ),
        )
      )
    )

  private def settingsDiv: ReactiveHtmlElement[HTMLDivElement] = {
    def settingsButton(
        name: String,
        onClickF: () => Unit,
        buttonClass: String = "btn-primary",
    ): ReactiveHtmlElement[HTMLDivElement] =
      div(
        className := "col-6 px-3 py-2 d-grid",
        button(
          className := "btn fs-4",
          className := buttonClass,
          `type` := "button",
          name,
          onClick --> { _ => onClickF() }
        ),
      )

    div(
      className := "row",
      settingsButton(
        "Manual Save",
        () =>
          actionUpdater.writer.onNext(
            _.tap(
              SaveLoad
                .saveToLocalStorage(_)
                .foreach(_ => currentGlobalAlert.set(Some("Game Saved!")))
            )
          )
      ),
      div(
        className := "col-6 px-3 py-2 d-grid",
        div(
          className := "d-flex justify-content-between align-items-center",
          "Auto-Save:",
          children <--
            autoSaveMode.signal.map { selectedSaveMode =>
              allSaveModes.map { saveMode =>
                button(
                  className := "btn fs-4",
                  className := (if (saveMode == selectedSaveMode) "btn-primary active"
                                else "btn-secondary"),
                  `type` := "button",
                  saveMode match {
                    case 0           => "Off"
                    case t if t < 60 => s"${t}s"
                    case t           => s"${t / 60}m"
                  },
                  onClick --> { _ => autoSaveMode.set(saveMode) }
                )
              }
            },
        ),
      ),
      settingsButton(
        "Load",
        () =>
          SaveLoad
            .loadFromLocalStorage()
            .foreach(SaveLoad.reloadDataFromLoadedSave(_))
      ),
      settingsButton(
        "Export",
        () =>
          actionUpdater.writer.onNext {
            _.tap { allData =>
              val dataStr = SaveLoad.saveString(allData)
              Option(dom.window.navigator.clipboard)
                .foreach(
                  _.writeText(dataStr).toFuture.map(_ =>
                    currentGlobalAlert.set(Some("Exported to clipboard!"))
                  )(queue)
                )
            }
          },
      ),
      settingsButton(
        "Import",
        () => {
          jquery.$(s"#${Constants.ImportModalId}").asInstanceOf[js.Dynamic].modal("show")
        },
      ),
      settingsButton(
        "Hard Reset",
        () => {
          jquery.$(s"#${Constants.NewGameModalId}").asInstanceOf[js.Dynamic].modal("show")
        },
        buttonClass = "btn-danger",
      ),
    )
  }

  private def buildQueueDiv: ReactiveHtmlElement[HTMLDivElement] = {
    def buildTaskName(buildTask: BuildTask, index: Int): ReactiveHtmlElement[HTMLSpanElement] = {
      span(
        buildTask.chamberType.name,
        " [",
        TickCalculator.calculateTicksToResources(buildTask.actionCost),
        "]",
        onClick --> { _ => Actions.removeBuildTask(index) },
      ).amendThis(elem =>
        createTooltipForActionButton(
          elem = elem,
          descriptionSignal = Val(None),
          costSignal = Val(buildTask.actionCost),
          bonusSignal = Actions.getActionBonus(buildTask.chamberType),
        )
      )
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
            case Seq() =>
              span(nbsp)
            case seq =>
              span(seq.zipWithIndex.map { case (buildTask, index) =>
                buildTaskName(buildTask, index)
              })
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
      className := "fs-5",
      width.px := ResourcesWidth,
      maxWidth.px := ResourcesWidth,
      div(
        className := "card-header",
        "Resources",
      ),
      div(
        className := "card-body",
        child.maybe <-- createResourceDiv(
          name = "Sugar",
          resourceSignal = sugarSignal,
          resourceEstimationSignal =
            Val(prettyResourceEstDiv(TickCalculator.sugarTickGain, sugarSignal)),
          showSignal = Val(true),
        ),
        child.maybe <-- createResourceDiv(
          name = "Tunneling Space",
          resourceSignal = tunnelingSpaceSignal,
          resourceEstimationSignal =
            Val(prettyResourceEstDiv(TickCalculator.tunnelingSpaceTickGain, tunnelingSpaceSignal)),
          showSignal = unlocksSignal.map(_.resources.showTunnelingSpace),
        ),
        child.maybe <-- createResourceDiv(
          name = "Colony Points",
          resourceSignal = colonyPointsSignal,
          resourceEstimationSignal =
            Val(prettyResourceEstDiv(TickCalculator.colonyPointsTickGain, colonyPointsSignal)),
          showSignal = unlocksSignal.map(_.resources.showColonyPoints),
        ),
        nbsp,
        child.maybe <-- createResourceDiv(
          name = "Eggs",
          resourceSignal = eggResourceSignal,
          resourceEstimationSignal = Val(div()),
          showSignal = unlocksSignal.map(_.resources.showEggs),
          tooltipOpt = Some(
            div(
              child <--
                antsSignal.map(_.eggsAndLarvae).map {
                  case Seq() =>
                    "No eggs"
                  case seq =>
                    div(
                      seq.map { antBlood =>
                        div(
                          b(antBlood.name + ": "),
                          child <-- currentTickSignal.map { currentTick =>
                            prettyTimeFromTicks(
                              (antBlood.initialTick + AntBrood.defaultTicksToGrow) - currentTick
                            )
                          },
                          " to grow to next stage."
                        )
                      }
                    )
                }
            )
          ),
        ),
        child.maybe <-- createResourceDiv(
          name = "Workers",
          resourceSignal = workersResourceSignal,
          resourceEstimationSignal = Val(div()),
          showSignal = unlocksSignal.map(_.resources.showWorkers)
        ),
      )
    )

  private def createResourceDiv(
      name: String,
      resourceSignal: Signal[BasicResource],
      resourceEstimationSignal: Signal[ReactiveHtmlElement[HTMLDivElement]],
      showSignal: Signal[Boolean],
      tooltipOpt: Option[ReactiveHtmlElement[HTMLDivElement]] = None,
  ): Signal[Option[ReactiveHtmlElement[HTMLDivElement]]] = {
    val amountSignal: Signal[Long] = resourceSignal.map(_.amount).distinct
    val maxAmountSignal: Signal[Long] = resourceSignal.map(_.maxAmount).distinct

    showSignal.distinct.map {
      case false =>
        None
      case true =>
        Some(
          div(
            className := "row",
            div(
              className := "col-4 text-nowrap",
              name,
            ).amendThis(elem => setTooltipOpt(elem, tooltipOpt)),
            div(
              className := "col-3 text-end text-nowrap",
              child <-- amountSignal.map(prettyNumberFixedSize),
            ).amendThis(elem => setTooltipOpt(elem, tooltipOpt)),
            div(
              className := "col-2 text-start text-body-tertiary text-nowrap ps-0",
              child <--
                maxAmountSignal.map { maxValue =>
                  if (maxValue == 0)
                    nbsp
                  else
                    span("/", prettyNumberSimple(maxValue))
                },
            ).amendThis(elem => setTooltipOpt(elem, tooltipOpt)),
            div(
              className := "col-3 text-end text-nowrap",
              child <-- resourceEstimationSignal
            )
          )
        )
    }
  }

  private def nestDiv(implicit owner: Owner): ReactiveHtmlElement[HTMLDivElement] = {
    def standardBuyChamber(
        name: String,
        chamberType: ChamberType,
        unlockF: ActionUnlocks => Boolean,
    ): Signal[Option[ReactiveHtmlElement[HTMLDivElement]]] =
      ifUnlockedOpt(unlocks => unlockF(unlocks.actions)) {
        val chamberSignal = Actions.getChamber(chamberType)
        val actionCost = Actions.buildChamberCost(chamberType)
        actionButton(
          name = name,
          topRightBadgeSignal =
            chamberSignal.map(_.level).distinct.map(Some(_).filter(_ > 0).map(span(_))),
          enabledSignal = Val(true),
          descriptionSignal = Val(None),
          costSignal = actionCost,
          bonusSignal = Actions.getActionBonus(chamberType),
          onClickAction = _ =>
            useSignalValue[ActionCost](
              actionCost,
              actionCost => Actions.addBuildTask(BuildTask(chamberType, actionCost))
            ),
        )
      }

    div(
      className := "row",
      actionButton(
        name = GatherSugarAction,
        enabledSignal = Val(true),
        descriptionSignal = Val(None),
        costSignal = Val(ActionCost.empty),
        bonusSignal = Val(ActionBonus(sugar = Constants.InitialGatherSugarAmount)),
        onClickAction = _ => giveResources(sugar = PlayerGatherSugarClickAmount),
      ),
      child.maybe <-- ifUnlockedOpt(unlocks =>
        unlocks.actions.canFeedQueen && !unlocks.actions.canLayEggs
      ) {
        actionButton(
          name = FeedQueenAction,
          enabledSignal = hasResourcesSignal(Val(Constants.InitialFeedQueenActionCost)),
          descriptionSignal = Val(Some(div("Feed the queen to lay eggs."))),
          costSignal = Val(Constants.InitialFeedQueenActionCost),
          bonusSignal = Val(ActionBonus(customText = "Unlocks Lay Egg action")),
          onClickAction = _ => unlocksData.update(_.modify(_.actions.canLayEggs).setTo(true)),
        )
      },
      child.maybe <-- ifUnlockedOpt(_.actions.canLayEggs) {
        actionButton(
          name = LayEggAction,
          topLeftBadgeSignal = modifiersMultiplierSignal.map {
            case 1    => None
            case mult => Some(span(s"x$mult"))
          },
          enabledSignal = Actions.layEggActionEnabled,
          descriptionSignal = Val(Some(div("Ants require a constant sugar supply to stay alive."))),
          costSignal = modifiersMultiplierSignal.map(mult =>
            ActionCost(sugar = mult * Constants.LayEggSugarCost)
          ),
          bonusSignal = modifiersMultiplierSignal.map(mult => ActionBonus(eggs = mult * u)),
          onClickAction = mult => Actions.layEggAction(mult),
        )
      },
      child.maybe <-- standardBuyChamber(
        name = ChamberType.Nest.name,
        chamberType = ChamberType.Nest,
        unlockF = _.canBuildNestUpgrade,
      ),
      child.maybe <-- standardBuyChamber(
        name = ChamberType.Queen.name,
        chamberType = ChamberType.Queen,
        unlockF = _.canBuildQueenChamber,
      ),
      child.maybe <-- standardBuyChamber(
        name = ChamberType.FoodStorage.name,
        chamberType = ChamberType.FoodStorage,
        unlockF = _.canBuildFoodStorageChamber,
      ),
    )
  }

  private def actionButton(
      name: String,
      enabledSignal: Signal[Boolean],
      descriptionSignal: Signal[Option[ReactiveHtmlElement[HTMLDivElement]]],
      costSignal: Signal[ActionCost],
      bonusSignal: Signal[ActionBonus],
      onClickAction: Int => Unit,
      topLeftBadgeSignal: Signal[Option[ReactiveHtmlElement[HTMLElement]]] = Val(None),
      topRightBadgeSignal: Signal[Option[ReactiveHtmlElement[HTMLElement]]] = Val(None),
  )(implicit owner: Owner): ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "col-6 px-3 py-2 d-grid",
      button(
        className := "btn btn-primary position-relative",
        className := "fs-4",
        `type` := "button",
        name,
        disabled <-- enabledSignal.map(!_),
        onClick --> { _ =>
          useSignalValue[Int](modifiersMultiplierSignal, mult => onClickAction(mult))
        },
        child.maybe <--
          topLeftBadgeSignal.map {
            _.map { elem =>
              span(
                className := "position-absolute top-0 start-0 translate-middle badge rounded-pill bg-secondary",
                elem,
              )
            }
          },
        child.maybe <--
          topRightBadgeSignal.map {
            _.map { elem =>
              span(
                className := "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-secondary",
                elem,
              )
            }
          },
      ),
    ).amendThis(elem =>
      createTooltipForActionButton(elem, descriptionSignal, costSignal, bonusSignal)
    )

  private def createTooltipForActionButton(
      elem: ReactiveHtmlElement[HTMLElement],
      descriptionSignal: Signal[Option[ReactiveHtmlElement[HTMLDivElement]]],
      costSignal: Signal[ActionCost],
      bonusSignal: Signal[ActionBonus],
  ): EventListener[MouseEvent, MouseEvent] =
    setTooltip(
      elem,
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
            ifGreater0(cost.tunnelingSpace)(prettyNumberSimple("", _, " tunneling space")),
            ifGreater0(cost.colonyPoints)(prettyNumberSimple("", _, " colony points")),
            ifGreater0(cost.idleWorkers)(prettyNumberSimple("", _, " idle workers")),
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
            ifNonEmpty(bonus.customText)(text => div(i(text))),
          )
        },
      )
    )

  private def tasksDiv: ReactiveHtmlElement[HTMLDivElement] =
    div(
      className := "d-grid gap-2",
      span(
        "Idle Ants: ",
        b(child <-- idleWorkersCountSignal.map(prettyNumberFixedSize)),
      ),
      children <-- unlockedAntTasks.map(_.map(taskDiv)),
    )

  private def taskDiv(antTask: AntTask): ReactiveHtmlElement[HTMLDivElement] = {
    val amountSignal: Signal[Long] =
      antsSignal.map(_.tasks.find(_._1 == antTask).map(_._2).getOrElse(0L)).distinct
    val id = s"task-${antTask.toString.toLowerCase}"

    // TODO: Improve this, add bonuses dynamically, instead of constants
    val tooltipSignal: ReactiveHtmlElement[HTMLDivElement] =
      antTask match {
        case AntTask.SugarCollector =>
          div(prettyNumberSimple("+", defaultTaskCollectSugarSecond, " sugar per second / ant"))
        case AntTask.Tunneler =>
          div(
            prettyNumberSimple(
              "+",
              defaultTaskTunnelingSpaceSecond,
              " tunneling space per second / ant"
            )
          )
        case AntTask.Explorer =>
          div("Ants exploring the surroundings.")
      }

    div(
      idAttr := id,
      className := "row",
      span(
        className := "col-6",
        antTask.name,
        nbsp,
        "(",
        child <-- amountSignal.map(prettyNumberInt),
        ")",
      ),
      div(
        className := "col-2",
        when(antTask.showButtons)(
          button(
            className := "btn btn-primary",
            className := "m-1",
            cursor.pointer,
            i(
              className := "fa-solid fa-minus",
            ),
            disabled <-- amountSignal.map(_ == 0),
            onClick --> { mouseEvent =>
              Actions.reduceAntTask(antTask, getMouseEventMultiplier(mouseEvent) * u)
            }
          ),
        ),
        when(antTask.showButtons)(
          button(
            className := "btn btn-primary",
            className := "m-1",
            cursor.pointer,
            i(
              className := "fa-solid fa-plus",
            ),
            disabled <-- idleWorkersCountSignal.map(_ == 0),
            onClick --> { mouseEvent =>
              Actions.incrementAntTask(antTask, getMouseEventMultiplier(mouseEvent) * u)
            }
          ),
        ),
      ),
      div(className := "col-4"),
    ).amendThis(elem => setTooltip(elem, tooltipSignal))
  }

  private def upgradesDiv(implicit owner: Owner): ReactiveHtmlElement[HTMLDivElement] = {
    def createUpgrade(
        upgradeType: UpgradeType,
        unlockFunc: UpgradesData => UpgradesData = identity,
    ): Signal[Option[ReactiveHtmlElement[HTMLDivElement]]] = {
      val costSignal: Signal[ActionCost] = Val(upgradeType.cost)
      val upgradeSignal: Signal[UpgradeData] = upgradesSignal.map(_(upgradeType))
      ifUpgradeReadyToBuyOpt(upgradeSignal) {
        actionButton(
          name = upgradeType.title,
          topRightBadgeSignal =
            costSignal.map(actionCost => Some(prettyNumberSimple(actionCost.colonyPoints))),
          enabledSignal = hasResourcesSignal(costSignal),
          descriptionSignal = Val(Some(div(upgradeType.description))),
          costSignal = costSignal,
          bonusSignal = Val(ActionBonus.empty),
          onClickAction = _ =>
            useSignalValue[ActionCost](
              costSignal,
              actionCost =>
                Actions.unlockUpgrade(actionCost, _.unlock(upgradeType).pipe(unlockFunc)),
            ),
        )
      }
    }

    div(
      className := "row",
      UpgradeType.all.map { upgradeType =>
        div(
          child.maybe <--
            createUpgrade(upgradeType)
        )
      },
    )
  }

  private def exploreDiv(implicit owner: Owner): ReactiveHtmlElement[HTMLDivElement] = {
    def createExploreArea(
        name: String,
        unlockFunc: Signal[Boolean],
        costSignal: Signal[ActionCost],
        description: Signal[Option[ReactiveHtmlElement[HTMLDivElement]]],
    ): Signal[Option[ReactiveHtmlElement[HTMLDivElement]]] =
      ifUnlockedOpt(unlockFunc) {
        actionButton(
          name = name,
          enabledSignal = hasResourcesSignal(costSignal).combineWith(explorationSignal).map {
            case (hasResources, exploration) =>
              hasResources && exploration.explorationParties.sizeIs < Constants.InitialMaxExplorationParties
          },
          descriptionSignal = description,
          costSignal = costSignal,
          bonusSignal = Val(ActionBonus.empty),
          onClickAction = _ =>
            useSignalValue[ActionCost](
              costSignal,
              actionCost => Actions.explore(actionCost),
            ),
        )
      }

    val amountOfExplorersVar: Var[Long] = Var(1L * u)

    div(
      className := "row",
      div(
        className := "col-12 px-3 py-2 d-grid",
        div(
          className := "row px-0",
          className := "d-flex justify-content-start align-items-center",
          div(
            className := "col-6",
            "Number of explorers to send:",
            nbsp,
            child <-- amountOfExplorersVar.signal.map(amount => b(prettyNumberInt(amount))),
          ),
          div(
            className := "col-6",
            input(
              className := "form-range",
              `type` := "range",
              MinAttr := "1",
              MaxAttr <-- maxWorkers.map(amount => (amount / u).toString),
              value <-- amountOfExplorersVar.signal.map(amount => (amount / u).toString),
              onInput --> { ev =>
                val value = ev.target.asInstanceOf[HTMLInputElement].value.toLong
                amountOfExplorersVar.set(value * u)
              },
            ),
          ),
        ),
      ),
      child.maybe <-- createExploreArea(
        name = "Explore Surroundings",
        Val(true),
        amountOfExplorersVar.signal.map(amount =>
          ActionCost(
            sugar = amount * Constants.InitialExplorationSugarCostPerWorker,
            idleWorkers = amount,
          )
        ),
        description = Val(Some(div("Send explorer ants out exploring the nest surroundings."))),
      ),
      div(
        className := "pt-3 px-3",
        "Number of exploration parties: ",
        child <-- explorationSignal.map(_.explorationParties.size),
        " / ",
        b(child.text <-- explorationSignal.map(_.maxExplorationsParties).distinct),
      ),
      div(
        className := "col-12 pt-3 d-grid",
        children <-- explorationSignal.map {
          _.explorationParties.map { case ExplorationParty(amountWorkers, _, targetTick) =>
            div(
              b(s"[${prettyNumberStr(amountWorkers)} Explorer Ant${plural(amountWorkers / u)}] "),
              child.text <-- currentTickSignal.map(targetTick - _).map(prettyTimeFromTicks),
            )
          }
        }
      )
    )
  }

}
