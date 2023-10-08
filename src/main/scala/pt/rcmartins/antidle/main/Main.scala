package pt.rcmartins.antidle.main

import com.raquo.laminar.api.L._
import org.scalajs.dom
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed(render(dom.document.getElementById("root"), MainForm()))

}
