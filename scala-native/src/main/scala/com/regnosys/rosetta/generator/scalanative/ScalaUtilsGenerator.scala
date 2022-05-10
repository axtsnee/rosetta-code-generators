package com.regnosys.rosetta.generator.scalanative

object ScalaUtilsGenerator {
  val generateFile: String =
    s"""package ${AbstractScalaGenerator.basePackage}
      |
      |import scala.annotation.tailrec
      |import scala.jdk.CollectionConverters._
      |import scala.util.{Try, Success, Failure}
      |
      |import com.google.inject.{Guice, Injector}
      |import com.regnosys.rosetta.common.validation.RosettaTypeValidator
      |import com.regnosys.rosetta.common.validation.ValidationReport
      |import com.rosetta.model.lib.RosettaModelObject
      |import org.isda.cdm.CdmRuntimeModule
      |
      |object implicits {
      |  implicit val injector: Injector =
      |    Guice.createInjector(new CdmRuntimeModule())
      |
      |  implicit def validator(implicit injector: Injector): RosettaTypeValidator =
      |    injector.getInstance(classOf[RosettaTypeValidator])
      |
      |  implicit def validateSingle(
      |    implicit validator: RosettaTypeValidator
      |  ): RosettaModelObject => ValidationReport =
      |    m => { validator.runProcessStep(m.getClass, m) }
      |
      |  implicit def validateList(
      |    implicit validator: RosettaTypeValidator
      |  ): java.util.List[_ <: RosettaModelObject] => ValidationReport = ms => {
      |    val failures = ms.asScala.flatMap { m =>
      |      validator
      |        .runProcessStep(m.getClass, m)
      |        .validationFailures
      |        .asScala
      |    }
      |    new ValidationReport(null, failures.asJava)
      |  }
      |}
      |
      |object Utils {
      |  def traverseTry[A, B](as: List[A])(f: A => Try[B]): Try[List[B]] = {
      |    @tailrec
      |    def loop(remaining: List[A], acc: List[B]): Try[List[B]] =
      |      remaining match {
      |        case Nil      => Success(acc)
      |        case hd :: tl =>
      |          f(hd) match {
      |            case Success(b) => loop(tl, b :: acc)
      |            case Failure(t) => Failure(t)
      |          }
      |      }
      |    loop(as, List.empty[B])
      |  }
      |
      |  def traverseTry[A, B](o: Option[A])(f: A => Try[B]): Try[Option[B]] =
      |    o match {
      |      case None    => Success(None)
      |      case Some(a) => f(a).map(Some.apply)
      |    }
      |}
      |""".stripMargin
}
