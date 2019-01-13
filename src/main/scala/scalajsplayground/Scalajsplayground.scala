package scalajsplayground

import java.util.UUID

import cats.effect.IO
import com.raquo.airstream.eventbus.WriteBus
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveChildNode
import google.maps.LatLng
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import magnolia._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html
import scalajsplayground.FormAnnotation._

import scala.language.experimental.macros
import scala.scalajs.js.annotation._



sealed trait Page {
  def prettyPrint: String
}

case object Home extends Page {
  override def prettyPrint: String = "Home"
}
case object Program extends Page {
  override def prettyPrint: String = "Program"
}
case object Info extends Page {
  override def prettyPrint: String = "Info"
}

case object Login extends Page {
  override def prettyPrint: String = "Login"
}

case object Billeder extends Page {
  override def prettyPrint: String = "Billeder"
}

case object Kort extends Page {
  override def prettyPrint: String = "Kort"
}

case object Play extends Page {
  override def prettyPrint: String = "Play"
}

case class S(
              age: Int,
              email: String,
              name: String,
              @Select(List(1.4F, 2.0F)) meh: Float,
              @Select(List("foo", "bar")) stuff: String)

sealed trait IceCream
case object Nougat extends IceCream
case object Vanilla extends IceCream


case class AppState(
    name: String,
    occupation: String,
    clicks: Int,
    lastResponse: Option[Post],
    currentPage: Page,
    currentPassword: String,
    imageLists: List[String],
    messages: List[String],
    submits: List[S]
)

object AppState {

  val pics = List(
    "https://docs.aws.amazon.com/AmazonS3/latest/dev/images/resource-based-policy.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/AWS_Simple_Icons_Storage_Amazon_S3.svg/1024px-AWS_Simple_Icons_Storage_Amazon_S3.svg.png"
  )

  def initial() = {
    AppState("", "", 1, None, Home, "", pics, Nil, Nil)
  }
}

case class Post(userId: Int, id: Int, title: String, body: String)

object Post {
  implicit val fooDecoder: Decoder[Post] = deriveDecoder[Post]
}

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
case object InitializeMap extends AppActions
case class MessageFromJS(message: String) extends AppActions
case class AttemptedSubmit(s: S) extends AppActions

@JSExportTopLevel("app")
object V2 extends ElmApp[AppState, AppActions](dom.document.querySelector("#app"), AppState.initial()) {

  def renderMap(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      h2(
        "Vi skal gå fra kirken til Ecco"
      ),
      div(
        id := "mapid",
        height := "500px"
      )
    )
  }

  def renderOvernatning(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]
  ): ReactiveChildNode[dom.Element] = {
    div(
      h1(
        "Overnatning"
      ),
      p(
        "Man kan overnatte på Ecco Conference center."
      ),
      p(
        "Overnatning er incl morgenmad og koster 24 kr."
      )
    )
  }

  def renderToastMaster(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]
  ): ReactiveChildNode[dom.Element] = {
    div(
      span(
        "The toast master is the venerable Bjarke."
      )
    )
  }

  def renderProgram(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    val program = List(
      "14:30 - 15:30" -> "Den kirkelige vielse",
      "15:30 - 15:45" -> "Champagne og fælles billede foran kirken",
      "17:00" -> "Velkomstsdrinks",
      "17:30" -> "Middagen starter",
      "??:??" -> "Der lukkes og slukkes"
    )
    div(
      className := "text-center",
      ul(
        className := "list-group justify-content-center",
        program.map {
          case (time, activity) =>
            li(
              className := "list-group-item text-center",
              h5(
                time
              ),
              p(
                activity
              )
            )
        }
      )
    )
  }

  def renderInfo(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      div(
        className := "card",
        div(
          className := "card-body",
          h5(
            className := "card-title",
            "Overnatning"
          ),
          p(
            className := "card-text",
            "Man kan overnatte på Ecco."
          ),
          p(
            className := "card-text",
            "Det koster penge."
          )
        )
      ),
      div(
        className := "card",
        div(
          className := "card-body",
          h5(
            className := "card-title",
            "Toast master"
          ),
          p(
            className := "card-text",
            "Toastmasteren hedder Bjarke."
          ),
          p(
            className := "card-text",
            "Han er gommens lillebror."
          )
        )
      ),
      div(
        className := "card",
        div(
          className := "card-body",
          h5(
            className := "card-title",
            "Ønskeseddel"
          ),
          p(
            className := "card-text",
            "Vi har valgt at ligge vores ønskeseddel i Ønskeskyen så vi undgår alt for mange af de samme gaver ."
          )
        )
      )
    )
  }

  def renderPlay(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {

    div(
      Input.gen[S].create[AppActions](s => AttemptedSubmit(s), bus, Nil) match {
        case NeedsName(f, valueExtractor) => throw new RuntimeException("WT?")
        case Done(d)                      => d
      }
    )
  }

  override def view(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      header(
        role := "banner",
        img(
          className := "centerImage",
          src := "https://i.pinimg.com/originals/fc/09/ef/fc09efe7456dcd5c333760dd7d364cc3.gif",
          width := "200px",
          id := "logo-main"
        ),
        div(
          className := "container",
          nav(
            className := "navbar navbar-expand-lg navbar-light bg-light",
            div(
              className := "collapse navbar-collapse",
              id := "navbarNav",
              ul(
                className := "navbar-nav",
                List(Home, Program, Info, Kort, Billeder, Play).map { page =>
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
              case Home     => renderHome(stateStream, bus)
              case Login    => renderLogin(stateStream, bus)
              case Program  => renderProgram(stateStream, bus)
              case Billeder => renderPictures(stateStream, bus)
              case Info     => renderInfo(stateStream, bus)
              case Kort     => renderMap(stateStream, bus)
              case Play     => renderPlay(stateStream, bus)
              case _        => div("HELLO!")
            }
          )
        )
      )
    )
  }

  def renderHome(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      div(
        "HELLLO"
      ),
      button(
        onClick.mapTo(CauseAlert) --> bus,
        "Cause alert"
      ),
      h1(
        "MessageList"
      ),
      div(
        children <-- stateStream.map(_.messages).map { msgList =>
          msgList.map { str =>
            div(str)
          }
        }
      )
    )
  }

  def renderLogin(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
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
          inContext(node => onInput.mapTo(UpdatePasswordInput(node.ref.value)) --> bus),
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

  def renderPictures(stateStream: L.Signal[AppState], bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      h3(
        "Når vi får billederne vil de komme her :)"
      ),
      div(
        id := "carousel",
        className := "carousel slide",
        dataAttr("ride") := "carousel",
        div(
          className := "carousel-inner",
          children <-- stateStream.map(_.imageLists).map { linkList =>
            linkList.zipWithIndex.map {
              case (imageLink, index) =>
                div(
                  className := s"""carousel-item ${if (index == 0) "active" else ""}""",
                  img(
                    src := imageLink
                  )
                )
            }
          }
        )
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

  override def update(state: AppState, action: AppActions): (AppState, List[IO[AppActions]]) = {

    val res = action match {
      case NameUpdated(updatedName)   => (state.copy(name = updatedName), Nil)
      case OccupationChanged(updated) => (state.copy(occupation = updated), Nil)
      case HeaderClick                => (state.copy(clicks = state.clicks + 1), Nil)
      case FetchPost =>
        (state, List(IO.fromFuture(IO(Ajax.get(s"http://jsonplaceholder.typicode.com/posts/${state.clicks}"))).map {
          resp =>
            PostResponse(decode[Post](resp.responseText))
        }))
      case PostResponse(either) =>
        (state.copy(lastResponse = either.toOption), Nil)
      case NavigateToPage(page) =>
        println(page)
        val actions = page match {
          case Kort => List(IO(InitializeMap))
          case _    => Nil
        }
        println(actions)
        (state.copy(currentPage = page), actions)
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
        println(Obj.v1())
        (state, Nil)
      case InitializeMap =>
        val opts = google.maps.MapOptions(
          center = new LatLng(54.9418395, 8.8610071),
          zoom = 14,
          panControl = false,
          streetViewControl = false,
          mapTypeControl = false
        )

        val gmap = new google.maps.Map(dom.document.querySelector("#mapid"), opts)

        val eccoPosition = new LatLng(54.933895, 8.842890)
        val churchPosition = new LatLng(54.936733, 8.870548)

        val churcMarker = new google.maps.Marker(
          google.maps.MarkerOptions(
            position = churchPosition,
            animation = google.maps.Animation.BOUNCE,
            map = gmap,
            title = "The church - the serious place"
          )
        )

        val eccoMarker = new google.maps.Marker(
          google.maps.MarkerOptions(
            position = eccoPosition,
            map = gmap,
            title = "Ecco - the party place",
          )
        )
        (state, Nil)
      case MessageFromJS(msg) =>
        println(s"Message from js: ${msg}")
        println(s"messages before: ${state.messages}")
        val messages = msg :: state.messages
        println(s"messages after: ${messages}")
        (state.copy(messages = messages), Nil)
      case AttemptedSubmit(s) =>
        (state.copy(submits = s :: state.submits), Nil)
    }

    println(s"$state +  $action -> $res")
    res
  }

  override def subs(subs: List[L.EventStream[AppActions]]): Unit = {}

  @JSExport
  def msg(input: String): Unit = {
    inbound(MessageFromJS(input))
  }

}

abstract class ElmApp[S, A](container: dom.Element, initialState: S) {

  val actionBus = new EventBus[A]

  def main(args: Array[String]): Unit = {

    val stateStream = actionBus.events.fold(initialState) { (state, action) =>
      val (newState, effects) = update(state, action)
      effects.foreach(_.unsafeRunAsync {
        case Left(error) =>
          println(s"EROROR: $error")
          println(s"ZOMG: ${error.getMessage}")
          println(error)
          error.printStackTrace()
        case Right(generatedAction) =>
          println(generatedAction)
          actionBus.writer.onNext(generatedAction)
      })
      newState
    }

    render(container, view(stateStream, actionBus.writer))
  }

  def view(stateStream: Signal[S], bus: WriteBus[A]): ReactiveChildNode[dom.Element]
  def update(state: S, action: A): (S, List[IO[A]])
  def subs(subs: List[EventStream[A]]): Unit

  def inbound(action: A): Unit = {
    actionBus.writer.onNext(action)
  }
}
