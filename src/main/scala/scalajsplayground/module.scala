package scalajsplayground

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("uuid", JSImport.Namespace)
@js.native
object Obj extends js.Object {
  def v1(): String = js.native
}


@JSImport("./assets/uuid-facade.js", JSImport.Namespace)
@js.native
object MapRenderer extends js.Object {
  def generate(): String = js.native
}