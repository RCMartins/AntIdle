package pt.rcmartins.antidle.utils

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLSpanElement
import pt.rcmartins.antidle.game.Constants.u

object UIUtils {

  def prettyNumber(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    if (num <= 9999 * u)
      if (num % u == 0)
        span((num / u).toString, span(opacity := 0, ".00K"))
      else {
        val shortValue = num / 10
        span((shortValue / 100).toString, ".", f"${shortValue % 100}%02d", span(opacity := 0, "K"))
      }
    else {
      span(">9999")
    }

  def prettyNumberInt(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    span((num / u).toString)

}
