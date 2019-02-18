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
import io.scalajs.nodejs._
import io.scalajs.npm.aws._
import io.scalajs.npm.aws.s3._

import scala.concurrent.Future
import scala.xml.Elem
import scala.xml.factory.XMLLoader

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

case class S(age: Int,
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

  def initial() = {
    AppState("", "", 1, None, Login, "", Nil, Nil, Nil)
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
case object FetchPictureLinks extends AppActions
case class PicturesResponse(links: List[String]) extends AppActions

@JSExportTopLevel("app")
object V2
    extends ElmApp[AppState, AppActions](dom.document.querySelector("#app"),
                                         AppState.initial()) {

  def renderMap(stateStream: L.Signal[AppState],
                bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
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

  def renderProgram(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    val program = List(
      ("14:30", "Kirkelig vielse", Some("Efter vielsen vil vi gerne tage et fælles billede ved kirken sammen med jer")),
      ("17:00", "Bryllupsfesten starter på Ecco med velkomstdrinks", None),
      ("17:30", "Vi går til bords", None),
      ("Søndag omkring kl. 10", "Vi spiser morgenmad sammen", None)
    )
    div(
      className := "text-center",
      ul(
        className := "list-group justify-content-center",
        program.map {
          case (time, activity, note) =>
            li(
              className := "list-group-item text-center",
              h5(
                time
              ),
              p(
                activity
              ),
              note.map { str =>
                p(
                  str
                )
              }
            )
        }
      )
    )
  }

  def renderInfo(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      div(
        className := "card",
        div(
          className := "card-body",
          h3(
            className := "card-title",
            "Overnatning"
          ),
          h5(
            className := "card-text",
            "Der er mulighed for at overnatte på Ecco Conference Center, og vi har reserveret værelser, så der er til alle."
          ),
          h5(
            className := "card-text",
            "Enkeltværelse: 785,-"
          ),
          h5(
            className := "card-text",
            "Dobbeltværelse 870,-"
          ),
          h5(
            className := "card-text",
            "Begge værelsespriser er inklusiv morgenmad. Hvis du gerne vil overnatte på feststedet, skal du senest den 1. april give besked til os. Ecco har bedt om en samlet faktura, og du skal derfor betale overnatningen via os. Så hvis du kunne tænke dig at overnatte, skal du overføre prisen for enkelt- eller dobbeltværelse til "
          ),
          h5(
            className := "card-text",
            "Mobilepay: 30 28 00 94"
          )
        )
      ),
      div(
        className := "card",
        div(
          className := "card-body",
          h3(
            className := "card-title",
            "Toastmaster"
          ),
          h5(
            className := "card-text",
            "Toastmasteren hedder Bjarke."
          ),
          h5(
            className := "card-text",
            "Han er gommens lillebror og kan træffes på følgende nummer 23 65 90 37."
          )
        )
      ),
      div(
        className := "card",
        div(
          className := "card-body",
          h3(
            className := "card-title",
            "Ønskeseddel"
          ),
          h5(
            className := "card-text",
            "Vi opretter en ønskeseddel i Ønskeskyen. Vi arbejder på sagen! Stay tuned :)"
          )
        )
      )
    )
  }

  def renderPlay(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {

    div(
      Input.gen[S].create[AppActions](s => AttemptedSubmit(s), bus, Nil) match {
        case NeedsName(f, valueExtractor) => throw new RuntimeException("WT?")
        case Done(d)                      => d
      }
    )
  }

  override def view(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
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
                List(Home, Program, Info, Kort, Billeder).map { page =>
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

  def renderHome(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      div(
        className := "card",
        div(
          className := "card-body",
          h5(
            className := "card-title",
            div(
              className := "container",
              div(
                className := "row no-gutters",
                div(
                  className := "col-1",
                  h5(
                    className := "card-title",
                    "Hvem:"
                  )
                ),
                div(
                  className := "col-6",
                  div(
                    className := "container",
                    div(
                      className := "row",
                      h5(
                        className := "card-text",
                        "Sofie & Rune"
                      )
                    )
                  )
                )
              )
            ),
          )
        )
      ),
      div(
        className := "card",
        div(
          className := "card-body",
          h5(
            className := "card-title",
            div(
              className := "container",
              div(
                className := "row no-gutters",
                div(
                  className := "col-1",
                  h5(
                    className := "card-title",
                    "Hvad:"
                  )
                ),
                div(
                  className := "col-6",
                  div(
                    className := "container",
                    div(
                      className := "row",
                      h5(
                        className := "card-text",
                        "Bryllup! :)"
                      )
                    )
                  )
                )
              )
            ),
          )
        )
      ),
      div(
        className := "card",
        div(
          className := "card-body",
          div(
            className := "container",
            div(
              className := "row no-gutters",
              div(
                className := "col-1",
                h5(
                  className := "card-title",
                  "Hvornår:"
                )
              ),
              div(
                className := "col-6",
                div(
                  className := "container",
                  div(
                    className := "row",
                    h5(
                      className := "card-text",
                      "3. august 2019 fra kl 14.30"
                    )
                  )
                )
              )
            )
          ),
        )
      ),
//      div(
//        className := "card",
//        div(
//          className := "card-body",
//          h5(
//            className := "card-title",
//            "Hvor: Tønder Kristkirke, Kirkepladsen 4, 6270 Tønder"
//          ),
//          h5(
//            className := "card-title",
//            "Ecco Conference Center, Ecco Alleen 4, 6270 Tønder"
//          )
//        )
//      ),
      div(
        className := "card",
        div(
          className := "card-body",
          div(
            className := "container",
            div(
              className := "row no-gutters",
              div(
                className := "col-1",
                h5(
                  className := "card-title",
                  "Hvor:"
                )
              ),

              div(
                className := "col-6",
                div(
                  className := "container",
                  div(
                    className := "row",
                    h5(
                      className := "card-text",
                      "Tønder Kristkirke - Kirkepladsen 4, 6270 Tønder"
                    )
                  ),
                  div(
                    className := "row",
                    h5(
                      className := "card-text",
                      "og"
                    )
                  ),
                  div(
                    className := "row",
                    h5(
                      className := "card-text",
                      "Ecco Conference Center - Ecco Alleen 4, 6270 Tønder"
                    )
                  ),
                )
              )
            )
          )
        )
      )
    )
  }

  def renderLogin(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      h1(
        "Velkommen! :)"
      ),
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
            placeholder := "Adgangskode"
          )
        ),
        button(
          tpe := "submit",
          className := "btn btn-primary",
          onClick.preventDefault.mapTo(AttemptLogin) --> bus,
          "Fortsæt"
        )
      )
    )
  }

  def renderPictures(
      stateStream: L.Signal[AppState],
      bus: L.WriteBus[AppActions]): ReactiveChildNode[dom.Element] = {
    div(
      h3(
        "Når vi får billederne efter bryllupet vil de komme ind her :)"
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
                  className := s"""carousel-item ${if (index == 0) "active"
                  else ""}""",
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
               PostResponse(decode[Post](resp.responseText))
             }))
      case PostResponse(either) =>
        (state.copy(lastResponse = either.toOption), Nil)
      case NavigateToPage(page) =>
        val actions = page match {
          case Kort => List(IO(InitializeMap))
          case _    => Nil
        }
        (state.copy(currentPage = page), actions)
      case UpdatePasswordInput(input) =>
        (state.copy(currentPassword = input), Nil)
      case AttemptLogin => {
        state.currentPage match {
          case Login =>
            if (state.currentPassword.toLowerCase == "greve")
              (state.copy(currentPage = Home), Nil)
            else (state, Nil)
          case _ => (state, Nil)
        }
      }
      case CauseAlert =>
        val baseUrl = "http://wedding-pictures-2019.s3.amazonaws.com"
        val fetchPictures = IO.fromFuture(IO(Ajax.get(
          baseUrl)))
          .map { resp =>
            resp.responseXML.getElementsByTagName("Key")
          }.map { nodeList =>
          val itemCount = nodeList.length
          val list = (0 until itemCount).map { i =>
            val node = nodeList(i)
            s"$baseUrl/${node.textContent}"
          }
          PicturesResponse(list.toList)
        }
        (state, List(fetchPictures))
      case InitializeMap =>
        val opts = google.maps.MapOptions(
          center = new LatLng(54.9418395, 8.8610071),
          zoom = 14,
          panControl = false,
          streetViewControl = false,
          mapTypeControl = false
        )

        val gmap =
          new google.maps.Map(dom.document.querySelector("#mapid"), opts)

        val eccoPosition = new LatLng(54.933895, 8.842890)
        val churchPosition = new LatLng(54.936733, 8.870548)

        val churcMarker = new google.maps.Marker(
          google.maps.MarkerOptions(
            position = churchPosition,
            animation = google.maps.Animation.BOUNCE,
            map = gmap,
            title = "Kirken - the serious place"
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
        val messages = msg :: state.messages
        (state.copy(messages = messages), Nil)
      case AttemptedSubmit(s) =>
        (state.copy(submits = s :: state.submits), Nil)

      case FetchPictureLinks =>
        val baseUrl = "http://wedding-pictures-2019.s3.amazonaws.com"
        val fetchPictures = IO.fromFuture(IO(Ajax.get(
          baseUrl)))
          .map { resp =>
            resp.responseXML.getElementsByTagName("Key")
          }.map { nodeList =>
          val itemCount = nodeList.length
          val list = (0 until itemCount).map { i =>
            val node = nodeList(i)
            s"$baseUrl/${node.textContent}"
          }
          PicturesResponse(list.toList)
        }
        (state, List(fetchPictures))
      case PicturesResponse(list) =>
        (state.copy(imageLists = state.imageLists ++ list), Nil)
    }

    res
  }

  override def subs(subs: List[L.EventStream[AppActions]]): Unit = {}

  @JSExport
  def msg(input: String): Unit = {
    inbound(MessageFromJS(input))
  }

  override def onLoad: List[AppActions] = List(FetchPictureLinks)

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

    onLoad.foreach(actionBus.writer.onNext)
  }

  def view(stateStream: Signal[S],
           bus: WriteBus[A]): ReactiveChildNode[dom.Element]
  def update(state: S, action: A): (S, List[IO[A]])
  def subs(subs: List[EventStream[A]]): Unit

  def inbound(action: A): Unit = {
    actionBus.writer.onNext(action)
  }

  def onLoad: List[A] = Nil
}
