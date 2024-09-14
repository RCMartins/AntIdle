package pt.rcmartins.antidle.game

import com.raquo.laminar.api.L.{u => _, _}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLSpanElement
import pt.rcmartins.antidle.game.Constants._

import scala.annotation.tailrec

object UINumbersUtils {

  private val Suffixes: IndexedSeq[String] = IndexedSeq("", "K", "M", "B", "T", "Q")

  def prettyNumberFixedSize(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    prettyNumberFixedSize("", num, 0)

  @tailrec
  private def prettyNumberFixedSize(
      prefix: String,
      num: Long,
      suffixNum: Int,
  ): ReactiveHtmlElement[HTMLSpanElement] =
    if (num <= 9999 * u)
      if (num % u == 0) {
        if (suffixNum == 0)
          span(prefix, (num / u).toString, span(opacity := 0, ".00K"))
        else
          span(prefix, (num / u).toString, span(opacity := 0, ".00"), Suffixes(suffixNum))
      } else {
        val shortValue = num / 100
        span(
          prefix,
          (shortValue / 100).toString,
          ".",
          f"${shortValue % 100}%02d",
          Suffixes(suffixNum),
        )
      }
    else
      prettyNumberFixedSize(prefix, num / 1000, suffixNum + 1)

  def prettyNumberStr(num: Long): String =
    prettyNumberStr("", num, 0)

  @tailrec
  private def prettyNumberStr(
      prefix: String,
      num: Long,
      suffix: Int,
  ): String =
    if (num <= 9999 * u)
      if (num % u == 0)
        prefix + (num / u).toString + Suffixes(suffix)
      else {
        val shortValue = num / 100
        prefix +
          (shortValue / 100).toString +
          "." +
          f"${shortValue % 100}%02d" +
          Suffixes(suffix)
      }
    else
      prettyNumberStr(prefix, num / 1000, suffix + 1)

  def prettyNumberSimple(
      prefix: String,
      num: Long,
      suffixStr: String,
  ): ReactiveHtmlElement[HTMLSpanElement] =
    prettyNumberSimple(prefix, num, 0, suffixStr)

  def prettyNumberSimple(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    prettyNumberSimple("", num, 0, "")

  @tailrec
  private def prettyNumberSimple(
      prefix: String,
      num: Long,
      suffixNum: Int,
      suffixStr: String,
  ): ReactiveHtmlElement[HTMLSpanElement] =
    if (num <= 9999 * u)
      if (num % u == 0)
        span(prefix, (num / u).toString, Suffixes(suffixNum), suffixStr)
      else {
        val shortValue = num / 100
        span(
          prefix,
          (shortValue / 100).toString,
          ".",
          f"${shortValue % 100}%02d",
          Suffixes(suffixNum),
          suffixStr,
        )
      }
    else
      prettyNumberSimple(prefix, num / 1000, suffixNum + 1, suffixStr)

//  def prettyNumber1d(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
//    if (num <= 9999 * u) {
//      val shortValue = num / 1000
//      span((shortValue / 10).toString, ".", f"${shortValue % 10}%01d")
//    } else
//      span(">9999")

  def prettyNumberInt(num: Long): ReactiveHtmlElement[HTMLSpanElement] =
    span((num / u).toString)

  def prettyTimeFromTicks(ticks: Long): String = {
    val seconds = Math.ceil(ticks / TicksPerSecond.toDouble).toLong
    val minutes = seconds / 60
    val hours = minutes / 60
    val days = hours / 24
    val weeks = days / 7
    val months = days / 30
    val years = days / 365

    if (years > 0)
      f"$years year${plural(years)}"
    else if (months > 0)
      f"$months month${plural(months)}"
    else if (weeks > 0)
      f"$weeks week${plural(weeks)}"
    else if (days > 0)
      f"$days day${plural(days)}"
    else if (hours > 0)
      f"$hours hour${plural(hours)}"
    else if (minutes > 0)
      f"$minutes minute${plural(minutes)}"
    else
      f"$seconds second${plural(seconds)}"
  }

  def plural(seconds: Long): String = if (seconds != 1) "s" else ""

}
