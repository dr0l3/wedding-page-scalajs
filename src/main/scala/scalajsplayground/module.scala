package scalajsplayground

import com.raquo.airstream.eventbus.WriteBus

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}

@JSImport("uuid", JSImport.Namespace)
@js.native
object Obj extends js.Object {
  def v1(): String = js.native
}
