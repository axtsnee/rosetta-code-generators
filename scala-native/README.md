# Scala Code Generator
Translates Rosetta into idiomatic Scala. The goal of this project is to
generate code whose features can be easily discoverable through IDE
autocompletion.

Rosetta namespaces are changed into Scala packages like so:
* Types in namespace x.y.z: org.isda.cdm.scalanative.y.z.types
* Enumerations in namespace x.y.z: org.isda.cdm.scalanative.y.z.enums
* Functions in namespace x.y.z: org.isda.cdm.scalanative.y.z.functions
* Metatypes in namespace x.y.z: stubbed out in org.isda.cdm.scalanative.y.z.metas
* Other Rosetta elements, like Synonyms, are currently unimplemented

## Enumerations
Rosetta enumerations are translated into sum types. For instance,
```rosetta
namespace cdm.test:
version "test"

enum TestEnum: <"Test enum description.">
    TestEnumValue1 <"Test enum value 1">
    TestEnumValue2 <"Test enum value 2">
```
becomes
```scala
package org.isda.cdm.scalanative.test.enums {
  /** Test enum description. */
  sealed trait TestEnum
  object TestEnum {
    /** Test enum value 1 */
    case object TestEnumValue1 extends TestEnum

    /** Test enum value 2 */
    case object TestEnumValue2 extends TestEnum
  }
}
```
Enumerations that extend other enumerations are more complicated - see the
code.

## Types
Each Rosetta type is translated into a trait with the same name, and a
case class implementation with `Default` prepended to the name. For example:
```rosetta
type TestType: <"Test type description.">
    testTypeValue1 string (1..1) <"Test string">
    testTypeValue2 string (0..1) <"Test optional string">
    testTypeValue3 string (0..*) <"Test string list">
    testTypeValue4 TestType2 (1..1) <"Test TestType2">
    testEnum TestEnum (0..1) <"Optional test enum">
```
becomes
```scala
/** Test type description. */
trait TestType {
  /** Test string */
  def testTypeValue1: String
  /** Test optional string */
  def testTypeValue2: Option[String]
  /** Test string list */
  def testTypeValue3: List[String]
  /** Test TestType2 */
  def testTypeValue4: TestType2
  /** Optional test enum */
  def testEnum: Option[TestEnum]
}
```
and
```scala
/** Test type description.
  *
  * @param testTypeValue1 Test string
  * @param testTypeValue2 Test optional string
  * @param testTypeValue3 Test string list
  * @param testTypeValue4 Test TestType2
  * @param testEnum Optional test enum
  */
final case class DefaultTestType(
  testTypeValue1: String,
  testTypeValue2: Option[String],
  testTypeValue3: List[String],
  testTypeValue4: TestType2,
  testEnum: Option[TestEnum]
)(
  implicit validator: RosettaTypeValidator
) extends TestType
```
Case class constructors currently require an implicit RosettaTypeValidator
that is invoked to make sure the arguments are valid. An exception is
thrown if they are not.

There is one special case: a type whose only fields are other types (not
primitives or enums) that have a `one-of` condition are translated into
sum types. This choice was made to reduce unnecessary nesting. For instance,
```rosetta
type TestSumType:
    testType1Value1 TestType (0..1) <"First optional value">
    testType2Value1 TestType2 (0..1) <"Second optional value">
    condition: one-of
```
becomes
```scala
sealed trait TestSumType
...
sealed trait TestType extends Parent with TestSumType ...
sealed trait TestType2 extends Parent2 with TestSumType ...
```

## Functions
This project does not generate functions with implementations. Instead,
it generates wrappers around Java implementations that are instantiated
through Guice. Scala function parameters are translated into Java. Java
function results are translated into Scala. The function invocation is
wrapped in a `Try`. For example,
```rosetta
func TestFunc: <"Description">
    inputs:
        n number (1..)
    output:
        result TestType (1..1)
```
becomes
```scala
/** Description */
object TestFunc {
  def apply(
      n: BigDecimal
  )(
      implicit injector: com.google.inject.Injector,
      validate: cdm.base.TestType => ValidationReport,
      convertToScala: cdm.base.TestType => Try[org.isda.cdm.scalanative.base.TestType]
  ): Try[BigDecimal] =
    Try(injector.getInstance(classOf[TestFunc]).evaluate(n.bigDecimal)).map(BigDecimal.apply)
}
```
NB: functions that return simple types do not require the implicit
`validate` or `convertToScala` parameters. Default instances of all implicit
parameters can be imported from org.isda.cdm.scalanative.implicits.

## Metatypes
Only stubs are generated.

## Synonyms, other Rosetta objects
Unimplemented.

## Usage
```scala
import org.isda.cdm.scalanative.product.template.types._
val et: EconomicTerms = DefaultEconomicTerms(...)
import org.isda.cdm.scalanative.product.common.functions.Qualify_AssetClass_InterestRate_Swap
import org.isda.cdm.scalanative.implicits._
val isIrs: Try[Boolean] = Qualify_AssetClass_InterestRate_Swap(et)
/* val isIrs: scala.util.Try[Boolean] = Success(true) */
```