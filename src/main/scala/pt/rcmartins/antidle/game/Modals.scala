package pt.rcmartins.antidle.game

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom._
import pt.rcmartins.antidle.game.UINumbersUtils.DataBSDismissAttr
import pt.rcmartins.antidle.game.Utils.{actionUpdater, unlocksOwner}
import pt.rcmartins.antidle.game.saves.SaveLoad

import scala.util.chaining.scalaUtilChainingOps

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
                  .foreach { loadedAllData =>
                    Utils.pause = true
                    unlocksOwner.killAll()
                    actionUpdater.writer.onNext(_ => loadedAllData)
                    actionUpdater.writer.onNext(_.tap(_ => UnlockUtils.checkUnlocks(unlocksOwner)))
                    Utils.pause = false
                  }
                dataVar.set("")
              },
              "Import"
            )
          )
        )
      )
    )
  }

}
