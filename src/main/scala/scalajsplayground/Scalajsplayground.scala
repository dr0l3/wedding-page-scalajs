package scalajsplayground

import cats.effect.IO
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.{ReactiveChildNode, ReactiveHtmlElement}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLDocument

import scala.language.experimental.macros
import scala.scalajs.js
import scala.scalajs.js.annotation._

class InputBox(val node: Div, val inputNode: Input)

object InputBox {
  def make(caption: String): InputBox = {
    val inputNode = input(typ := "text")
    val node = div(caption, inputNode)
    new InputBox(node, inputNode)
  }
}

sealed trait Page {
  def prettyPrint: String
}

case object Home extends Page {
  override def prettyPrint: String = "Home"
}
case object Features extends Page {
  override def prettyPrint: String = "Features"
}
case object Pricing extends Page {
  override def prettyPrint: String = "Pricing"
}

case object Login extends Page {
  override def prettyPrint: String = "Login"
}

case object Picture extends Page {
  override def prettyPrint: String = "Pictures"
}

case object Gifts extends Page {
  override def prettyPrint: String = "Gifts"
}


case class AppState(name: String,
                    occupation: String,
                    clicks: Int,
                    lastResponse: Option[Post],
                    currentPage: Page,
                    currentPassword: String,
                    imageLists: List[String])

object AppState {
  val pics = List(
    "https://docs.aws.amazon.com/AmazonS3/latest/dev/images/resource-based-policy.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/AWS_Simple_Icons_Storage_Amazon_S3.svg/1024px-AWS_Simple_Icons_Storage_Amazon_S3.svg.png"
  )
  def initial() = {
    AppState("", "", 1, None, Home, "", pics)
  }
}

case class Post(userId: Int, id: Int, title: String, body: String)

sealed trait AppActions
case class NameUpdated(name: String) extends AppActions
case class OccupationChanged(occupation: String) extends AppActions
case object HeaderClick extends AppActions
case object FetchPost extends AppActions
case class PostResponse(res: Either[Throwable, Post]) extends AppActions
case class NavigateToPage(page: Page) extends AppActions
case class UpdatePasswordInput(pw: String) extends AppActions
case object AttemptLogin extends AppActions
case object CauseAlert extends AppActions

class InputGroup(state: Signal[AppState],
                 bus: WriteBus[AppActions],
                 preTexts: List[String],
                 onInputF: String => AppActions) {
  def render(): ReactiveHtmlElement[html.Div] = {
    div(
      className := "input-group",
      div(
        className := "input-group-prepend",
        preTexts.map(
          text =>
            span(
              className := "input-group-text",
              text
          ))
      ),
      input(
        className := "form-control",
        inContext(thisNode =>
          onInput.mapTo(onInputF.apply(thisNode.ref.value)) --> bus),
        typ := "text"
      )
    )
  }
}



object V2
    extends ElmApp[AppState, AppActions](dom.document.querySelector("#app"),
                                         AppState.initial()) {
  override def view(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {

    div(
      className := "container",
      nav(
        className := "navbar navbar-expand-lg navbar-light bg-light",
        div(
          className := "collapse navbar-collapse",
          id := "navbarNav",
          ul(
            className := "navbar-nav",
            List(Home, Features, Pricing, Picture, Gifts).map { page =>
              li(
                className := s"nav-item",
                a(
                  className := "nav-link",
                  href := "#",
                  onClick.mapTo(NavigateToPage(page)) --> bus,
                  page.prettyPrint,
                  span(
                    className := "sr-only"
                  )
                )
              )
            }
          )
        )
      ),
      div(
        child <-- stateStream.map(_.currentPage).map {
          case Home    => renderHome(stateStream, bus)
          case Login   => renderLogin(stateStream, bus)
          case Picture => renderPictures(stateStream, bus)
          case Gifts => div("Gifts")
          case _       => div("HELLO!")
        }
      )
    )
  }

  def renderHome(stateStream: L.Signal[AppState],
                 bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      div(
        "HELLLO"
      ),
      button(
        onClick.mapTo(CauseAlert) --> bus,
        "Cause alert"
      ),
      div(
        height := "100px",
        id := "mapid"
      )
    )
  }

  def renderLogin(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    form(
      div(
        className := "form-group",
        label(
          forId := "exampleInputPassword",
          "Enter the magic key"
        ),
        input(
          tpe := "password",
          className := "form-control",
          id := "exampleInputPassword",
          inContext(
            node => onInput.mapTo(UpdatePasswordInput(node.ref.value)) --> bus),
          placeholder := "Password"
        )
      ),
      button(
        tpe := "submit",
        className := "btn btn-primary",
        onClick.preventDefault.mapTo(AttemptLogin) --> bus,
        "go"
      )
    )
  }

  def renderPictures(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      id := "carousel",
      className := "carousel slide",
      dataAttr("ride") := "carousel",
      div(
        className := "carousel-inner",
        inContext(node =>
          child <-- stateStream.map(_.imageLists).map { linkList =>
            linkList.zipWithIndex.foldLeft(node) { (acc, next) =>
              acc.appendChild(
                div(
                  className := s"""carousel-item ${if (next._2 == 0) "active"
                  else ""}""",
                  img(
                    src := next._1
                  )
                )
              )
              acc
            }
        })
      ),
      a(
        className := "carousel-control-prev",
        href := "#carousel",
        role := "button",
        dataAttr("slide") := "prev",
        span(
          className := "carousel-control-prev-icon",
          aria.hidden := true
        ),
        span(
          className := "sr-only",
          "Previous"
        )
      ),
      a(
        className := "carousel-control-next",
        href := "#carousel",
        role := "button",
        dataAttr("slide") := "next",
        span(
          className := "carousel-control-prev-icon",
          aria.hidden := true
        ),
        span(
          className := "sr-only",
          "Next"
        )
      )
    )
  }

  override def update(state: AppState,
                      action: AppActions): (AppState, List[IO[AppActions]]) = {

    val res = action match {
      case NameUpdated(updatedName)   => (state.copy(name = updatedName), Nil)
      case OccupationChanged(updated) => (state.copy(occupation = updated), Nil)
      case HeaderClick                => (state.copy(clicks = state.clicks + 1), Nil)
      case FetchPost =>
        (state,
         List(
           IO.fromFuture(IO(Ajax.get(
               s"http://jsonplaceholder.typicode.com/posts/${state.clicks}")))
             .map { resp =>
               implicit val fooDecoder: Decoder[Post] = deriveDecoder[Post]

               PostResponse(decode[Post](resp.responseText))
             }))
      case PostResponse(either) =>
        (state.copy(lastResponse = either.toOption), Nil)
      case NavigateToPage(page) => (state.copy(currentPage = page), Nil)
      case UpdatePasswordInput(input) =>
        (state.copy(currentPassword = input), Nil)
      case AttemptLogin => {
        state.currentPage match {
          case Login =>
            if (state.currentPassword == "SIMSAM")
              (state.copy(currentPage = Home), Nil)
            else (state, Nil)
          case _ => (state, Nil)
        }
      }
      case CauseAlert =>
        println("CAUING ALERT")
        println(Obj.v1())
//        println(Obj2.bar2())
//        println(MapRenderer.generate())
        (state, Nil)
    }

    println(s"$state +  $action -> $res")
    res
  }

  override def subs(subs: List[L.EventStream[AppActions]]): Unit = {}
}

abstract class ElmApp[S, A](container: dom.Element, initialState: S) {
  def main(args: Array[String]): Unit = {
    val actionBus = new EventBus[A]

    val stateStream = actionBus.events.fold(initialState) { (state, action) =>
      val (newState, effects) = update(state, action)
      effects.foreach(_.unsafeRunAsync {
        case Left(error) =>
          error.printStackTrace()
        case Right(generatedAction) =>
          actionBus.writer.onNext(generatedAction)
      })
      newState
    }

    render(container, view(stateStream, actionBus.writer))
  }

  def view(stateStream: Signal[S],
           bus: WriteBus[A]): ReactiveChildNode[dom.Element]
  def update(state: S, action: A): (S, List[IO[A]])
  def subs(subs: List[EventStream[A]]): Unit
}
