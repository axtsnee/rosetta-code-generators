package com.regnosys.rosetta.generator.scalawrapper

import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScalaNativeGeneratorTest extends AnyFlatSpec with Matchers {
  import Fixtures._

  "The ScalaNativeGenerator" should "handle simple enums" in {
    val actual = ParserHelper.parse(cdmTestEnum).get(ScalaNativeGenerator.enumFilename)
    val expected =
      """package org.isda.scala.test.enums {
        |
        |/** Test enum description. */
        |sealed trait TestEnum
        |object TestEnum {
        |  /** Test enum value 1 */
        |  case object TestEnumValue1 extends TestEnum
        |  /** Test enum value 2 */
        |  case object TestEnumValue2 extends TestEnum
        |
        |  implicit class JavaConverter(x: TestEnum) {
        |    def asJava: test.TestEnum = x match {
        |      case TestEnumValue1 => test.TestEnum.TEST_ENUM_VALUE_1
        |      case TestEnumValue2 => test.TestEnum.TEST_ENUM_VALUE_2
        |    }
        |  }
        |
        |  implicit class ScalaConverter(x: test.TestEnum) {
        |    def asScala: TestEnum = x match {
        |      case test.TestEnum.TEST_ENUM_VALUE_1 => TestEnumValue1
        |      case test.TestEnum.TEST_ENUM_VALUE_2 => TestEnumValue2
        |    }
        |  }
        |}
        |
        |}
        |""".stripMargin
    actual.value.toString should endWith(expected)
  }

  it should "handle one-of enums part 1" in {
    val actual = ParserHelper.parse(cdmTestOneOfType + cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected = "sealed trait TestSumType"
    actual.value.toString should include(expected)
  }

  it should "handle one-of enums part 2" in {
    val actual = ParserHelper.parse(cdmTestOneOfType + cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""/** Test type description. */
         |trait TestType extends TestSumType {
         |  /** Test string */
         |  def testTypeValue1: String
         |  /** Test optional string */
         |  def testTypeValue2: Option[String]
         |  /** Test string list */
         |  def testTypeValue3: List[String]
         |  /** Test TestType2 */
         |  def testTypeValue4: TestType2
         |  /** Optional test enum */
         |  def testEnum: Option[TestEnum]
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle one-of enums part 3" in {
    val actual = ParserHelper.parse(cdmTestOneOfType + cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""trait TestType2 extends TestSumType {
         |  /** Test number list */
         |  def testType2Value1: List[BigDecimal]
         |  /** Test date */
         |  def testType2Value2: Option[LocalDate]
         |  /** Test enum */
         |  def testEnum: TestEnum
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle simple types part 1" in {
    val actual = ParserHelper.parse(cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""/** Test type description. */
         |trait TestType {
         |  /** Test string */
         |  def testTypeValue1: String
         |  /** Test optional string */
         |  def testTypeValue2: Option[String]
         |  /** Test string list */
         |  def testTypeValue3: List[String]
         |  /** Test TestType2 */
         |  def testTypeValue4: TestType2
         |  /** Optional test enum */
         |  def testEnum: Option[TestEnum]
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle simple types part 2" in {
    val actual = ParserHelper.parse(cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""/** Test type description.
         |  *
         |  * @param testTypeValue1 Test string
         |  * @param testTypeValue2 Test optional string
         |  * @param testTypeValue3 Test string list
         |  * @param testTypeValue4 Test TestType2
         |  * @param testEnum Optional test enum
         |  */
         |final case class DefaultTestType(
         |  testTypeValue1: String,
         |  testTypeValue2: Option[String],
         |  testTypeValue3: List[String],
         |  testTypeValue4: TestType2,
         |  testEnum: Option[TestEnum]
         |)(implicit validator: RosettaTypeValidator) extends TestType {
         |  private[types] val toJava: test.TestType = {
         |    val unvalidated = TestType.buildJava(this)
         |    val validation = validator.runProcessStep(unvalidated.getClass, unvalidated.toBuilder)
         |    if (validation.success) unvalidated
         |    else throw new IllegalStateException(validation.validationFailures.asScala.mkString("; "))
         |  }
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle simple types part 3" in {
    val actual = ParserHelper.parse(cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""trait TestType2 {
         |  /** Test number list */
         |  def testType2Value1: List[BigDecimal]
         |  /** Test date */
         |  def testType2Value2: Option[LocalDate]
         |  /** Test enum */
         |  def testEnum: TestEnum
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle simple types part 4" in {
    val actual = ParserHelper.parse(cdmTestEnum + cdmTestType + cdmTestType2).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""/**
         |  * @param testType2Value1 Test number list
         |  * @param testType2Value2 Test date
         |  * @param testEnum Test enum
         |  */
         |final case class DefaultTestType2(
         |  testType2Value1: List[BigDecimal],
         |  testType2Value2: Option[LocalDate],
         |  testEnum: TestEnum
         |)(implicit validator: RosettaTypeValidator) extends TestType2""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle hierarchical types part 1" in {
    val actual = ParserHelper.parse(cdmTestHierarchy).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""trait TestType2 extends TestType3 {
         |  /** Test number */
         |  def testType2Value1: Option[BigDecimal]
         |  /** Test date */
         |  def testType2Value2: List[LocalDate]
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle hierarchical types part 2" in {
    val actual = ParserHelper.parse(cdmTestHierarchy).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""trait TestType3 {
         |  /** Test string */
         |  def testType3Value1: Option[String]
         |  /** Test int */
         |  def testType3Value2: List[Int]
         |}
         |""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle hierarchical types part 3" in {
    val actual = ParserHelper.parse(cdmTestHierarchy).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""final case class DefaultTestType(
         |  testType3Value1: Option[String],
         |  testType3Value2: List[Int],
         |  testType2Value1: Option[BigDecimal],
         |  testType2Value2: List[LocalDate],
         |  testType1Value1: String,
         |  testType1Value2: Option[Int]
         |)(implicit validator: RosettaTypeValidator) extends TestType""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle hierarchical types part 4" in {
    val actual = ParserHelper.parse(cdmTestHierarchy).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""final case class DefaultTestType2(
         |  testType3Value1: Option[String],
         |  testType3Value2: List[Int],
         |  testType2Value1: Option[BigDecimal],
         |  testType2Value2: List[LocalDate]
         |)(implicit validator: RosettaTypeValidator) extends TestType2""".stripMargin
    actual.value.toString should include(expected)
  }

  it should "handle hierarchical types part 5" in {
    val actual = ParserHelper.parse(cdmTestHierarchy).get(ScalaNativeGenerator.typeFilename)
    val expected =
      s"""final case class DefaultTestType3(
         |  testType3Value1: Option[String],
         |  testType3Value2: List[Int]
         |)(implicit validator: RosettaTypeValidator) extends TestType3""".stripMargin
    actual.value.toString should include(expected)
  }
}

object Fixtures {
  val cdmTestType: String =
    s"""type TestType: <"Test type description.">
       |\ttestTypeValue1 string (1..1) <"Test string">
       |\ttestTypeValue2 string (0..1) <"Test optional string">
       |\ttestTypeValue3 string (0..*) <"Test string list">
       |\ttestTypeValue4 TestType2 (1..1) <"Test TestType2">
       |\ttestEnum TestEnum (0..1) <"Optional test enum">
       |
       |""".stripMargin

  val cdmTestType2: String =
    s"""type TestType2:
       |\ttestType2Value1 number(1..*) <"Test number list">
       |\ttestType2Value2 date(0..1) <"Test date">
       |\ttestEnum TestEnum (1..1) <"Test enum">
       |
       |""".stripMargin
  val cdmTestEnum: String =
    s"""enum TestEnum: <"Test enum description.">
       |\tTestEnumValue1 <"Test enum value 1">
       |\tTestEnumValue2 <"Test enum value 2">
       |
       |""".stripMargin

  val cdmTestHierarchy: String =
    s"""type TestType extends TestType2:
       |\ttestType1Value1 string (1..1) <"Test string">
       |\ttestType1Value2 int (0..1) <"Test int">
       |
       |type TestType2 extends TestType3:
       |\ttestType2Value1 number (0..1) <"Test number">
       |\ttestType2Value2 date (0..*) <"Test date">
       |
       |type TestType3:
       |\ttestType3Value1 string (0..1) <"Test string">
       |\ttestType3Value2 int (1..*) <"Test int">
       |""".stripMargin

  val cdmTestOneOfType: String =
    s"""type TestSumType:
       |\ttestType1Value1 TestType (0..1) <"First optional value">
       |\ttestType2Value1 TestType2 (0..1) <"Second optional value">
       |\tcondition: one-of
       |""".stripMargin
}