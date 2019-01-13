package scalajsplayground

import java.util.UUID

import com.raquo.airstream.eventbus.WriteBus
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveChildNode
import magnolia._
import org.scalajs.dom
import org.scalajs.dom.html
import scalajsplayground.FormAnnotation.Select

import scala.annotation.{Annotation, StaticAnnotation}
import scala.language.experimental.macros

sealed trait FormAnnotation extends StaticAnnotation
object FormAnnotation {
  final case class Select[A](values: List[A]) extends FormAnnotation
}


object Example {
  case class Example(email: String, age: Int, @Select(List("Nougat", "Vanilla")) iceCream: String)

  val bus: WriteBus[Example] = ???

  val ageInput = input(
    typ := "number",
    className := "form-control"
  )

  val emailInput = input(
    typ := "email",
    className := "form-control"
  )

  val iceCreamInput = select(
    option(
      "Nougat"
    ),
    option(
      "Vanilla"
    )
  )

  val v = form(
    div(
      className := "form-group",
      emailInput
    ),
    div(
      className := "form-group",
      ageInput
    ),
    div(
      className := "form-group",
      iceCreamInput
    ),
    button(
      typ := "button",
      onClick.mapToValue(
        Example(
          emailInput.ref.value,
          ageInput.ref.value.toInt,
          iceCreamInput.ref.options(iceCreamInput.ref.selectedIndex).text
        )
      ) --> bus
    )
  )
}

trait Selectable[A] {
  def enumerateValues(): List[String]
  def select(str: String): Selectable[A]
  def getSelected(): Option[A]
}

object Selectable {

  def create[A](list: List[A]): Selectable[A] = {
    new Selectable[A] {
      var chosen = list.headOption

      override def enumerateValues(): List[String] = list.map(_.toString)

      override def select(str: String): Selectable[A] = {
        chosen = list.find(_.toString == str)
        this
      }

      override def getSelected(): Option[A] = {
        chosen
      }
    }
  }
}

sealed trait FormReturn[A]
case class NeedsName[A](f: String => ReactiveChildNode[dom.Element], valueExtractor: () => A) extends FormReturn[A]
case class Done[A](d: ReactiveChildNode[dom.Element]) extends FormReturn[A]

trait Input[C] {
  def create[A](onClick: C => A, bus: WriteBus[A], selectValues: List[C]): FormReturn[C]
}

object Input {
  type Email = String
  type Number = Int

  type Typeclass[T] = Input[T]

  implicit def selectForAllA[AA](implicit selectable: Selectable[AA]): Typeclass[Selectable[AA]] =
    new Typeclass[Selectable[AA]] {
      override def create[A](onClick: Selectable[AA] => A, bus: WriteBus[A], selectValues: List[Selectable[AA]]): FormReturn[Selectable[AA]] = {
        val selectId = UUID.randomUUID().toString
        val options = selectable.enumerateValues().map { v =>
          option(
            v
          )
        }

        val selectNode = select(
          className := "form-control",
          id := selectId,
          options
        )

        def creator(name: String) = {
          div(
            className := "form-group",
            label(
              forId := selectId,
              name
            ),
            selectNode
          )
        }
        NeedsName[Selectable[AA]](creator, () => selectable.select(selectNode.ref.value))
      }
    }

  implicit val inputString: Typeclass[String] = new Typeclass[String] {
    override def create[A](handler: String => A, bus: WriteBus[A], selectValues: List[String]): FormReturn[String] = {
      val inputId = UUID.randomUUID().toString

      selectValues match {
        case Nil =>
          val inputNode: ReactiveChildNode[html.Input] = input(
            id := inputId,
            typ := "text",
            className := "form-control"
          )

          val creator: String => ReactiveChildNode[html.Div] = name => {
            div(
              className := "form-group",
              label(
                forId := inputId,
                name
              ),
              inputNode
            )
          }
          NeedsName[String](f = creator, valueExtractor = () => inputNode.ref.value)
        case ::(head, tl) =>
          val options = head :: tl map { optText =>
            option(
              optText
            )
          }

          val inputNode =  select (
            id := inputId,
            className := "form-control",
            options
          )

          val creator: String => ReactiveChildNode[html.Div] = name => {
            div(
              className := "form-group",
              label(
                forId := inputId,
                name
              ),
              inputNode
            )
          }
          NeedsName[String](f = creator, valueExtractor = () => inputNode.ref.options(inputNode.ref.selectedIndex).text)
      }
    }
  }

  implicit val inputNum: Typeclass[Number] = new Typeclass[Number] {
    override def create[A](onClick: Number => A, bus: WriteBus[A], selectValues: List[Number]): FormReturn[Number] = {
      val inputId = UUID.randomUUID().toString
      selectValues match {
        case Nil =>
          val inputNode: ReactiveChildNode[html.Input] = input(
            id := inputId,
            typ := "number",
            className := "form-control"
          )
          val creator: String => ReactiveChildNode[dom.Element] = str => {
            div(
              className := "form-group",
              label(
                forId := inputId,
                str
              ),
              inputNode
            )
          }
          NeedsName[Number](creator, () => inputNode.ref.value.toInt)
        case ::(head, tl) =>
          val options = head :: tl map { optText =>
            option(
              optText.toString
            )
          }

          val inputNode =  select (
            id := inputId,
            className := "form-control",
            options
          )

          val creator: String => ReactiveChildNode[html.Div] = name => {
            div(
              className := "form-group",
              label(
                forId := inputId,
                name
              ),
              inputNode
            )
          }
          NeedsName[Number](f = creator, valueExtractor = () => inputNode.ref.options(inputNode.ref.selectedIndex).text.toInt)
      }

    }
  }

  implicit val inputFloat: Typeclass[Float] = new Typeclass[Float] {
    override def create[A](onClick: Float => A, bus: WriteBus[A], selectValues: List[Float]): FormReturn[Float] = {
      val inputId = UUID.randomUUID().toString
      selectValues match {
        case Nil =>
          val inputNode: ReactiveChildNode[html.Input] = input(
            id := inputId,
            typ := "number",
            className := "form-control"
          )
          val creator: String => ReactiveChildNode[dom.Element] = str => {
            div(
              className := "form-group",
              label(
                forId := inputId,
                str
              ),
              inputNode
            )
          }
          NeedsName[Float](creator, () => inputNode.ref.value.toInt)
        case ::(head, tl) =>
          val options = head :: tl map { optText =>
            option(
              optText.toString
            )
          }

          val inputNode =  select (
            id := inputId,
            className := "form-control",
            options
          )

          val creator: String => ReactiveChildNode[html.Div] = name => {
            div(
              className := "form-group",
              label(
                forId := inputId,
                name
              ),
              inputNode
            )
          }
          NeedsName[Float](f = creator, valueExtractor = () => inputNode.ref.options(inputNode.ref.selectedIndex).text.toFloat)
      }

    }
  }

  def combine[T](ctx: CaseClass[Input, T]): Input[T] = new Typeclass[T] {
    override def create[A](handler: T => A, bus: WriteBus[A], selectValues: List[T]): FormReturn[T] = {
      val (paramNodes, extractors) = ctx.parameters.map { param =>
        val selectValues = param.annotations.collectFirst {
          case Select(values) => values
        }

        param.typeclass.create(null, bus, selectValues.getOrElse(Nil).asInstanceOf[List[param.PType]]) match {
          case NeedsName(f, extractor) => f(param.label) -> extractor
          case Done(_) =>
            throw new RuntimeException(
              s"Expected form return of type NeedsName, but got Done. ParamLabel: ${param.label}"
            )
        }
      }.unzip

      val ret = form(
        paramNodes,
        button(
          typ := "button",
          onClick.mapTo(handler.apply(ctx.rawConstruct(extractors.map(ext => ext.apply())))) --> bus,
          "Go"
        )
      )

      Done(ret)
    }
  }

  implicit def gen[T]: Input[T] = macro Magnolia.gen[T]
}
