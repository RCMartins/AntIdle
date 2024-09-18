package pt.rcmartins.antidle.game

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom._
import pt.rcmartins.antidle.game.UIUtils.DataBSDismissAttr
import pt.rcmartins.antidle.game.saves.SaveLoad
import pt.rcmartins.antidle.model.{AllData, WorldData}

object Modals {

  def importModal(): ReactiveHtmlElement[HTMLDivElement] = {
    val dataVar: Var[String] = Var("")
    div(
      className := "modal fade",
      idAttr := Constants.ImportModalId,
      tabIndex(-1),
      div(
        className("modal-dialog modal-lg"),
        div(
          className := "modal-content",
          div(
            className := "modal-header",
            h5(className := "modal-title", "Import"),
            button(
              typ("button"),
              className("btn-close"),
              DataBSDismissAttr("modal"),
            ),
          ),
          div(
            className := "modal-body",
            p("Paste your save data here:"),
            textArea(
              className := "form-control",
              placeholder := "Save data",
              rows := 10,
              cols := 50,
              value <-- dataVar.signal,
            ).amendThis { elem =>
              onInput.mapTo(elem.ref.value) --> dataVar.writer
            }
          ),
          div(
            className := "modal-footer",
            button(
              typ := "button",
              className := "btn btn-secondary",
              DataBSDismissAttr("modal"),
              onClick --> { _ =>
                SaveLoad
                  .loadString(dataVar.now())
                  .foreach(SaveLoad.reloadDataFromLoadedSave(_))
                dataVar.set("")
              },
              "Import",
            )
          )
        )
      )
    )
  }

  def newGameModal(): ReactiveHtmlElement[HTMLDivElement] = {
    val dataVar: Var[String] = Var("")
    div(
      className := "modal fade",
      idAttr := Constants.NewGameModalId,
      tabIndex(-1),
      div(
        className("modal-dialog modal-lg"),
        div(
          className := "modal-content",
          div(
            className := "modal-header",
            h5(className := "modal-title", "Hard Reset"),
            button(
              typ("button"),
              className("btn-close"),
              DataBSDismissAttr("modal"),
            ),
          ),
          div(
            className := "modal-body",
            p(b("Resets EVERYTHING and starts a new game.")),
            p(b("This is not a prestige reset!")),
            p("Are you sure you want to continue?"),
          ),
          div(
            className := "modal-footer",
            button(
              typ := "button",
              className := "btn btn-danger",
              DataBSDismissAttr("modal"),
              onClick --> { _ =>
                SaveLoad.reloadDataFromLoadedSave(
                  AllData.initial.copy(world = WorldData.initial(System.currentTimeMillis())),
                  Some("New Game Started!"),
                )
                dataVar.set("")
              },
              "Confirm",
            )
          )
        )
      )
    )
  }

}
