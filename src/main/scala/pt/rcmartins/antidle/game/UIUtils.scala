package pt.rcmartins.antidle.game

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.{html, HTMLDivElement, HTMLSpanElement, MouseEvent}
import pt.rcmartins.antidle.main.MainForm.currentGlobalAlert

import scala.scalajs.js.timers.setTimeout

object UIUtils {

  val DataBSDismissAttr: HtmlAttr[String] = htmlAttr("data-bs-dismiss", StringAsIsCodec)
  val MinAttr: HtmlAttr[String] = htmlAttr("min", StringAsIsCodec)
  val MaxAttr: HtmlAttr[String] = htmlAttr("max", StringAsIsCodec)

  def createShowGlobalAlertsDiv(): ReactiveHtmlElement[html.Div] = {
    def alertDiv(
        customClassName: String,
        content: ReactiveHtmlElement[HTMLSpanElement],
    ): ReactiveHtmlElement[HTMLDivElement] =
      div(
        className := "alert alert-dismissible fade show",
        className := "position-absolute top-0 start-50 translate-middle-x",
        className := customClassName,
        role := "alert",
        content,
        button(
          className := "btn-close",
          onClick.mapTo(None) --> currentGlobalAlert
        )
      )

    div(
      child.maybe <-- currentGlobalAlert.signal.map {
        case Some(updateText) =>
          val validTimeout: Var[Boolean] = Var(true)
          Some(
            alertDiv(
              customClassName = "alert-success",
              content = span(className := "px-3", updateText),
            ).amend(
              onMountCallback(_ =>
                setTimeout(Constants.AlertTimeout) {
                  if (validTimeout.now())
                    currentGlobalAlert.set(None)
                }
              ),
              onUnmountCallback(_ => validTimeout.set(false))
            )
          )
        case _ =>
          None
      }
    )
  }

  def getMouseEventMultiplier(event: MouseEvent): Long =
    (if (event.ctrlKey) 10L else 1L) *
      (if (event.shiftKey) 25L else 1L) *
      (if (event.altKey) 100L else 1L)

}
