package com.regnosys.rosetta.generator.scalawrapper

object CdmUtilsGenerator {
  val generateFile: String =
    """import scala.annotation.tailrec
      |import scala.jdk.CollectionConverters._
      |import scala.util.{Try, Success, Failure}
      |
      |import com.regnosys.rosetta.common.validation.RosettaTypeValidator
      |import com.regnosys.rosetta.common.validation.ValidationReport
      |import com.rosetta.model.lib.RosettaModelObject
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
      |
      |  implicit def validateSingle(
      |    implicit validator: RosettaTypeValidator
      |  ): RosettaModelObject => ValidationReport =
      |    m => { validator.runProcessStep(m.getClass, m.toBuilder) }
      |
      |  implicit def validateList(
      |    implicit validator: RosettaTypeValidator
      |  ): java.util.List[_ <: RosettaModelObject] => ValidationReport = ms => {
      |    val failures = ms.asScala.flatMap { m =>
      |      validator
      |        .runProcessStep(m.getClass, m.toBuilder)
      |        .validationFailures
      |        .asScala
      |    }
      |    new ValidationReport(null, failures.asJava)
      |  }
      |
      |  implicit val noOp: AnyRef => ValidationReport =
      |    _ => { new ValidationReport(null, java.util.Collections.emptyList) }
      |}
      |""".stripMargin
}
