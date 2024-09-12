package pt.rcmartins.antidle.game

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLSpanElement
import pt.rcmartins.antidle.game.Constants.u

object UINumbersUtils {

  def prettyNumber(
      prefix: String,
      num: Long,
      suffix: String,
  ): ReactiveHtmlElement[HTMLSpanElement] =
    if (num <= 9999 * u)
      if (num % u == 0)
        span(prefix, (num / u).toString, span(opacity := 0, ".00K"), suffix)
      else {
        val shortValue = num / 100
        span(
          prefix,
          (shortValue / 100).toString,
          ".",
          f"${shortValue % 100}%02d",
          span(opacity := 0, "K"),
          suffix,
        )
      }
    else {
      span(prefix, ">9999", suffix)
    }

  def prettyNumber(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    prettyNumber("", num, "")

  def prettyNumberSimple(
      prefix: String,
      num: Long,
      suffix: String,
  ): ReactiveHtmlElement[HTMLSpanElement] =
    if (num <= 9999 * u)
      if (num % u == 0)
        span(prefix, (num / u).toString, suffix)
      else {
        val shortValue = num / 100
        span(
          prefix,
          (shortValue / 100).toString,
          ".",
          f"${shortValue % 100}%02d",
          suffix,
        )
      }
    else {
      span(prefix, ">9999", suffix)
    }

  def prettyNumberSimple(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    prettyNumberSimple("", num, "")

  def prettyNumber1d(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    if (num <= 9999 * u) {
      val shortValue = num / 1000
      span((shortValue / 10).toString, ".", f"${shortValue % 10}%01d")
    } else {
      span(">9999")
    }

  def prettyNumberInt(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    span((num / u).toString)

}
