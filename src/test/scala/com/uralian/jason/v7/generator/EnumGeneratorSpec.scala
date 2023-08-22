package com.uralian.jason.v7.generator

import com.uralian.jason.util.JsonUtils
import com.uralian.jason.v7.ast.{Annotation, JSEnum}
import com.uralian.jason.{AbstractUnitSpec, InternalMethodTester}
import org.scalacheck.Gen
import treehugger.forest._
import definitions._
import org.json4s._
import treehuggerDSL._

/**
 * EnumGenerator test suite.
 */
class EnumGeneratorSpec extends AbstractUnitSpec with InternalMethodTester {

  import EnumDefinitionBuilder._
  import ResolverBuilder._
  import SerializerBuilder._

  val simpleJsonSymbolGen = Gen.oneOf(Symbols.jstring,
    Symbols.jint, Symbols.jlong, Symbols.jdecimal, Symbols.jdouble)

  val simpleJsonTypeGen = simpleJsonSymbolGen.map(TYPE_REF)

  val simpleSymbolGen = Gen.oneOf(StringClass, BigIntClass, LongClass, BigDecimalClass, DoubleClass)

  val simpleTypeGen = simpleSymbolGen.map(TYPE_REF)

  private val someAnn = Some(Annotation(title = Some("My Enum"), description = Some("My enumeration")))

  private def funcTree(builder: SerializerBuilder, serializer: Boolean, typeName: String = "MyEnum") = {
    val funcName = if (serializer) "serFunc" else "desFunc"
    val desFunc = InternalMethod[Tree](Symbol(funcName))
    builder invokePrivate desFunc(typeName)
  }

  private def assertTreesEqual(actual: Tree, expected: Tree) =
    treeToString(actual) mustBe treeToString(expected)

  "SimpleSerializerBuilder" should {
    "render deserializer function" in
      forAll(simpleJsonTypeGen) { jsonType =>
        val actual = funcTree(SimpleSerializerBuilder(jsonType), false)
        val expected = BLOCK(
          CASE(jsonType UNAPPLY ID("x")) ==> REF("withValue") APPLY REF("x")
        )
        assertTreesEqual(actual, expected)
      }
    "render serializer function" in
      forAll(simpleJsonTypeGen) { jsonType =>
        val actual = funcTree(SimpleSerializerBuilder(jsonType), true)
        val expected = BLOCK(
          CASE(ID("data") withType "MyEnum") ==> jsonType APPLY (REF("data") DOT "value")
        )
        assertTreesEqual(actual, expected)
      }
    "render custom serializer" in
      forAll(simpleJsonTypeGen) { jsonType =>
        val typeName = "MyEnum"
        val builder = SimpleSerializerBuilder(jsonType)
        val actual = builder.render(typeName)
        val expected = {
          val desFunc = funcTree(builder, false)
          val serFunc = funcTree(builder, true)
          val lambda = LAMBDA(PARAM(WILDCARD)) ==> TUPLE(desFunc, serFunc)
          VAL("serializer") := NEW(Symbols.customSer APPLYTYPE typeName APPLY lambda)
        }
        assertTreesEqual(actual, expected)
      }
  }

  "CompoundSerializerBuilder" should {
    "render deserializer function" in {
      val actual = funcTree(CompoundSerializerBuilder(), false)
      val expected = BLOCK(
        CASE(ID("jv") withType Symbols.jvalue) ==> REF("withValue") APPLY REF("jv")
      )
      assertTreesEqual(actual, expected)
    }
    "render serializer function" in {
      val actual = funcTree(CompoundSerializerBuilder(), true)
      val expected = BLOCK(
        CASE(ID("data") withType "MyEnum") ==> REF("data") DOT "value"
      )
      assertTreesEqual(actual, expected)
    }
    "render custom serializer" in {
      val typeName = "MyEnum"
      val builder = CompoundSerializerBuilder()
      val actual = builder.render(typeName)
      val expected = {
        val desFunc = funcTree(builder, false)
        val serFunc = funcTree(builder, true)
        val lambda = LAMBDA(PARAM(WILDCARD)) ==> TUPLE(desFunc, serFunc)
        VAL("serializer") := NEW(Symbols.customSer APPLYTYPE typeName APPLY lambda)
      }
      assertTreesEqual(actual, expected)
    }
  }

  "SimpleResolverBuilder" should {
    "render resolver for simple enums" in
      forAll(simpleTypeGen) { valueType =>
        val typeName = "MyEnum"
        val builder = SimpleResolverBuilder(valueType)
        val actual = builder.render(typeName, List("Entry1", "Entry2"))
        val expected = DEF("withValue", typeName)
          .withParams(PARAM("x", valueType))
          .withAnnots(annotations) := REF("x") MATCH List(
          CASE(REF("Entry1") DOT "value") ==> REF("Entry1"),
          CASE(REF("Entry2") DOT "value") ==> REF("Entry2"),
          CASE(WILDCARD) ==> THROW(IllegalArgumentExceptionClass, REF("x"))
        )
        assertTreesEqual(actual, expected)
      }
  }

  "CompoundResolverBuilder" should {
    "render resolver for compound enums" in {
      val typeName = "MyEnum"
      val builder = CompoundResolverBuilder()
      val actual = builder.render(typeName, List("Entry1", "Entry2"))
      val expected = DEF("withValue", typeName)
        .withParams(PARAM("x", Symbols.jvalue))
        .withAnnots(annotations) := REF("x") MATCH List(
        CASE(REF("Entry1") DOT "value") ==> REF("Entry1"),
        CASE(REF("Entry2") DOT "value") ==> REF("Entry2"),
        CASE(WILDCARD) ==> THROW(IllegalArgumentExceptionClass, REF(Symbols.jsonMethods) DOT "pretty" APPLY
          (REF(Symbols.jsonMethods) DOT "render" APPLY REF("x")))
      )
      assertTreesEqual(actual, expected)
    }
  }

  "EnumDefinitionBuilder" should {
    "render imports" in {
      val renderImports = InternalMethod[List[Import]](Symbol("renderImports"))
      val actual = StringEnumBuilder invokePrivate renderImports()
      val expected = List(IMPORT(Symbols.json4s, "_"), IMPORT(Symbols.json4sMethods))
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
  }

  "StringEnumBuilder" should {
    "build distinct entry names" in {
      val method = InternalMethod[String](Symbol("buildEntryName"))
      StringEnumBuilder invokePrivate method("MyEnum", 0, JString("blah")) mustBe "Blah"
      StringEnumBuilder invokePrivate method("MyEnum", 1, JString("blah?")) mustBe "Blah1"
      StringEnumBuilder invokePrivate method("MyEnum", 2, JString("blah+ ")) mustBe "Blah2"
    }
    "render entry definition" in {
      val typeName = "MyEnum"
      val description = "My enum info"
      val renderEntryDefinition = InternalMethod[Tree](Symbol("renderEntryDefinition"))
      val actual = StringEnumBuilder invokePrivate renderEntryDefinition(typeName, Some(description))
      val expected = CLASSDEF(typeName)
        .withFlags(Flags.SEALED, Flags.ABSTRACT)
        .withParams(VAL("value", StringClass)).tree
        .withDoc(description, DocTag.Param("value", "entry value."))
      assertTreesEqual(actual, expected)
    }
    "render enum constants" in {
      val typeName = "MyEnum"
      val renderEnumConstants = InternalMethod[Seq[Tree]](Symbol("renderEnumConstants"))
      val actual = StringEnumBuilder invokePrivate renderEnumConstants(typeName, List(
        "Aaa" -> JString("AAA"), "Bbb" -> JString("BBB")
      ))
      val expected = List(
        CASEOBJECTDEF("Aaa").withParents(REF(typeName) APPLY LIT("AAA")),
        CASEOBJECTDEF("Bbb").withParents(REF(typeName) APPLY LIT("BBB"))
      )
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
    "render companion object" in {
      val renderCompanionDefinition = InternalMethod[Tree](Symbol("renderCompanionDefinition"))
      val tree = StringEnumBuilder invokePrivate renderCompanionDefinition("MyEnum",
        List(JString("AAA"), JString("BBB")))
      val actual = treeToString(tree)
      val expected = io.Source.fromResource("enum/string_enum_object.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }

  "IntEnumBuilder" should {
    "build distinct entry names" in {
      val method = InternalMethod[String](Symbol("buildEntryName"))
      IntEnumBuilder invokePrivate method("MyEnum", 0, JInt(123)) mustBe "MyEnum_123"
      IntEnumBuilder invokePrivate method("MyEnum", 1, JInt(456)) mustBe "MyEnum_456"
      IntEnumBuilder invokePrivate method("MyEnum", 2, JInt(-78)) mustBe "MyEnum_N78"
    }
    "render entry definition" in {
      val typeName = "MyEnum"
      val description = "My enum info"
      val renderEntryDefinition = InternalMethod[Tree](Symbol("renderEntryDefinition"))
      val actual = IntEnumBuilder invokePrivate renderEntryDefinition(typeName, Some(description))
      val expected = CLASSDEF(typeName)
        .withFlags(Flags.SEALED, Flags.ABSTRACT)
        .withParams(VAL("value", BigIntClass)).tree
        .withDoc(description, DocTag.Param("value", "entry value."))
      assertTreesEqual(actual, expected)
    }
    "render enum constants" in {
      val typeName = "MyEnum"
      val renderEnumConstants = InternalMethod[Seq[Tree]](Symbol("renderEnumConstants"))
      val actual = IntEnumBuilder invokePrivate renderEnumConstants(typeName, List(
        "Aaa" -> JInt(-123), "Bbb" -> JInt(456)
      ))
      val expected = List(
        CASEOBJECTDEF("Aaa").withParents(REF(typeName) APPLY (REF(BigIntClass) APPLY LIT("-123"))),
        CASEOBJECTDEF("Bbb").withParents(REF(typeName) APPLY (REF(BigIntClass) APPLY LIT("456")))
      )
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
    "render companion object" in {
      val renderCompanionDefinition = InternalMethod[Tree](Symbol("renderCompanionDefinition"))
      val tree = IntEnumBuilder invokePrivate renderCompanionDefinition("MyEnum",
        List(JInt(-123), JInt(456)))
      val actual = treeToString(tree)
      val expected = io.Source.fromResource("enum/int_enum_object.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }

  "LongEnumBuilder" should {
    "build distinct entry names" in {
      val method = InternalMethod[String](Symbol("buildEntryName"))
      LongEnumBuilder invokePrivate method("MyEnum", 0, JLong(123)) mustBe "MyEnum_123"
      LongEnumBuilder invokePrivate method("MyEnum", 1, JLong(456)) mustBe "MyEnum_456"
      LongEnumBuilder invokePrivate method("MyEnum", 2, JLong(-78)) mustBe "MyEnum_N78"
    }
    "render entry definition" in {
      val typeName = "MyEnum"
      val description = "My enum info"
      val renderEntryDefinition = InternalMethod[Tree](Symbol("renderEntryDefinition"))
      val actual = LongEnumBuilder invokePrivate renderEntryDefinition(typeName, Some(description))
      val expected = CLASSDEF(typeName)
        .withFlags(Flags.SEALED, Flags.ABSTRACT)
        .withParams(VAL("value", LongClass)).tree
        .withDoc(description, DocTag.Param("value", "entry value."))
      assertTreesEqual(actual, expected)
    }
    "render enum constants" in {
      val typeName = "MyEnum"
      val renderEnumConstants = InternalMethod[Seq[Tree]](Symbol("renderEnumConstants"))
      val actual = LongEnumBuilder invokePrivate renderEnumConstants(typeName, List(
        "Aaa" -> JLong(-123), "Bbb" -> JLong(456)
      ))
      val expected = List(
        CASEOBJECTDEF("Aaa").withParents(REF(typeName) APPLY (REF(LongClass) APPLY LIT(-123L))),
        CASEOBJECTDEF("Bbb").withParents(REF(typeName) APPLY (REF(LongClass) APPLY LIT(456L)))
      )
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
    "render companion object" in {
      val renderCompanionDefinition = InternalMethod[Tree](Symbol("renderCompanionDefinition"))
      val tree = LongEnumBuilder invokePrivate renderCompanionDefinition("MyEnum",
        List(JLong(-123), JLong(456)))
      val actual = treeToString(tree)
      val expected = io.Source.fromResource("enum/long_enum_object.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }

  "DecimalEnumBuilder" should {
    "build distinct entry names" in {
      val method = InternalMethod[String](Symbol("buildEntryName"))
      DecimalEnumBuilder invokePrivate method("MyEnum", 0, JDecimal(12.3)) mustBe "MyEnum_12_3"
      DecimalEnumBuilder invokePrivate method("MyEnum", 1, JDecimal(4.56)) mustBe "MyEnum_4_56"
      DecimalEnumBuilder invokePrivate method("MyEnum", 2, JDecimal(-7.8)) mustBe "MyEnum_N7_8"
    }
    "render entry definition" in {
      val typeName = "MyEnum"
      val description = "My enum info"
      val renderEntryDefinition = InternalMethod[Tree](Symbol("renderEntryDefinition"))
      val actual = DecimalEnumBuilder invokePrivate renderEntryDefinition(typeName, Some(description))
      val expected = CLASSDEF(typeName)
        .withFlags(Flags.SEALED, Flags.ABSTRACT)
        .withParams(VAL("value", BigDecimalClass)).tree
        .withDoc(description, DocTag.Param("value", "entry value."))
      assertTreesEqual(actual, expected)
    }
    "render enum constants" in {
      val typeName = "MyEnum"
      val renderEnumConstants = InternalMethod[Seq[Tree]](Symbol("renderEnumConstants"))
      val actual = DecimalEnumBuilder invokePrivate renderEnumConstants(typeName, List(
        "Aaa" -> JDecimal(12.3), "Bbb" -> JDecimal(-4.56)
      ))
      val expected = List(
        CASEOBJECTDEF("Aaa").withParents(REF(typeName) APPLY (REF(BigDecimalClass) APPLY LIT("12.3"))),
        CASEOBJECTDEF("Bbb").withParents(REF(typeName) APPLY (REF(BigDecimalClass) APPLY LIT("-4.56")))
      )
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
    "render companion object" in {
      val renderCompanionDefinition = InternalMethod[Tree](Symbol("renderCompanionDefinition"))
      val tree = DecimalEnumBuilder invokePrivate renderCompanionDefinition("MyEnum",
        List(JDecimal(1.23), JDecimal(-45.6)))
      val actual = treeToString(tree)
      val expected = io.Source.fromResource("enum/decimal_enum_object.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }

  "DoubleEnumBuilder" should {
    "build distinct entry names" in {
      val method = InternalMethod[String](Symbol("buildEntryName"))
      DoubleEnumBuilder invokePrivate method("MyEnum", 0, JDouble(12.3)) mustBe "MyEnum_12_3"
      DoubleEnumBuilder invokePrivate method("MyEnum", 1, JDouble(4.56)) mustBe "MyEnum_4_56"
      DoubleEnumBuilder invokePrivate method("MyEnum", 2, JDouble(-7.8)) mustBe "MyEnum_N7_8"
    }
    "render entry definition" in {
      val typeName = "MyEnum"
      val description = "My enum info"
      val renderEntryDefinition = InternalMethod[Tree](Symbol("renderEntryDefinition"))
      val actual = DoubleEnumBuilder invokePrivate renderEntryDefinition(typeName, Some(description))
      val expected = CLASSDEF(typeName)
        .withFlags(Flags.SEALED, Flags.ABSTRACT)
        .withParams(VAL("value", DoubleClass)).tree
        .withDoc(description, DocTag.Param("value", "entry value."))
      assertTreesEqual(actual, expected)
    }
    "render enum constants" in {
      val typeName = "MyEnum"
      val renderEnumConstants = InternalMethod[Seq[Tree]](Symbol("renderEnumConstants"))
      val actual = DoubleEnumBuilder invokePrivate renderEnumConstants(typeName, List(
        "Aaa" -> JDouble(12.3), "Bbb" -> JDouble(-4.56)
      ))
      val expected = List(
        CASEOBJECTDEF("Aaa").withParents(REF(typeName) APPLY (REF(DoubleClass) APPLY LIT(12.3))),
        CASEOBJECTDEF("Bbb").withParents(REF(typeName) APPLY (REF(DoubleClass) APPLY LIT(-4.56)))
      )
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
    "render companion object" in {
      val renderCompanionDefinition = InternalMethod[Tree](Symbol("renderCompanionDefinition"))
      val tree = DoubleEnumBuilder invokePrivate renderCompanionDefinition("MyEnum",
        List(JDouble(1.23), JDouble(-45.6)))
      val actual = treeToString(tree)
      val expected = io.Source.fromResource("enum/double_enum_object.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }

  "JValueEnumBuilder" should {
    "build distinct entry names" in {
      val method = InternalMethod[String](Symbol("buildEntryName"))
      JValueEnumBuilder invokePrivate method("MyEnum", 0, JString("abc")) mustBe "MyEnum_0"
      JValueEnumBuilder invokePrivate method("MyEnum", 1, JInt(456)) mustBe "MyEnum_1"
      JValueEnumBuilder invokePrivate method("MyEnum", 2, JBool(true)) mustBe "MyEnum_2"
    }
    "render entry definition" in {
      val typeName = "MyEnum"
      val description = "My enum info"
      val renderEntryDefinition = InternalMethod[Tree](Symbol("renderEntryDefinition"))
      val actual = JValueEnumBuilder invokePrivate renderEntryDefinition(typeName, Some(description))
      val expected = CLASSDEF(typeName)
        .withFlags(Flags.SEALED, Flags.ABSTRACT)
        .withParams(VAL("value", Symbols.jvalue)).tree
        .withDoc(description, DocTag.Param("value", "entry value."))
      assertTreesEqual(actual, expected)
    }
    "render enum constants" in {
      val typeName = "MyEnum"
      val renderEnumConstants = InternalMethod[Seq[Tree]](Symbol("renderEnumConstants"))
      val actual = JValueEnumBuilder invokePrivate renderEnumConstants(typeName, List[(String, JValue)](
        "E1" -> JString("Hello"),
        "E2" -> JInt(123),
        "E3" -> JLong(456L),
        "E4" -> JDecimal(4.567),
        "E5" -> JDouble(456.7),
        "E6" -> JNull,
        "E7" -> JBool(true),
        "E8" -> JsonUtils.parseJson("""[1, 2, {"a": false}]"""),
        "E9" -> JsonUtils.parseJson("""{"x": [true, 2]}""")
      ))
      val expected = List(
        CASEOBJECTDEF("E1").withParents(REF(typeName) APPLY (REF(Symbols.jstring) APPLY LIT("Hello"))),
        CASEOBJECTDEF("E2").withParents(REF(typeName) APPLY (REF(Symbols.jint) APPLY (REF(BigIntClass) APPLY LIT("123")))),
        CASEOBJECTDEF("E3").withParents(REF(typeName) APPLY (REF(Symbols.jlong) APPLY LIT(456L))),
        CASEOBJECTDEF("E4").withParents(REF(typeName) APPLY (REF(Symbols.jdecimal) APPLY (REF(BigDecimalClass) APPLY LIT("4.567")))),
        CASEOBJECTDEF("E5").withParents(REF(typeName) APPLY (REF(Symbols.jdouble) APPLY LIT(456.7))),
        CASEOBJECTDEF("E6").withParents(REF(typeName) APPLY REF(Symbols.jnull)),
        CASEOBJECTDEF("E7").withParents(REF(typeName) APPLY (REF(Symbols.jbool) APPLY LIT(true))),
        CASEOBJECTDEF("E8").withParents(REF(typeName) APPLY (REF(Symbols.jsonMethods) DOT "parse" APPLY LIT("""[1,2,{"a":false}]"""))),
        CASEOBJECTDEF("E9").withParents(REF(typeName) APPLY (REF(Symbols.jsonMethods) DOT "parse" APPLY LIT("""{"x":[true,2]}""")))
      )
      assertTreesEqual(BLOCK(actual), BLOCK(expected))
    }
    "render companion object" in {
      val renderCompanionDefinition = InternalMethod[Tree](Symbol("renderCompanionDefinition"))
      val tree = JValueEnumBuilder invokePrivate renderCompanionDefinition("MyEnum", List(
        JString("Hello"),
        JInt(123),
        JLong(456L),
        JDecimal(4.567),
        JDouble(456.7),
        JNull,
        JBool(true),
        JsonUtils.parseJson("""[1, 2, {"a": false}]"""),
        JsonUtils.parseJson("""{"x": [true, 2]}""")
      ))
      val actual = treeToString(tree)
      val expected = io.Source.fromResource("enum/jvalue_enum_object.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }

  "EnumGenerator" should {
    "generate enum for JString items" in {
      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
      val enum = JSEnum(annotation = someAnn, List[JValue](
        JString("abc"), JString("ABC"), JString("x:y")
      ))
      val actual = treeToString(gen.generateDefinition("MyEnum", enum))
      val expected = io.Source.fromResource("enum/string_def.gen").getLines().mkString("\n")
      actual mustBe expected
    }
    "generate enum for JInt items" in {
      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
      val enum = JSEnum(annotation = someAnn, List[JValue](
        JInt(123), JInt(0), JInt(-456)
      ))
      val actual = treeToString(gen.generateDefinition("MyEnum", enum))
      val expected = io.Source.fromResource("enum/int_def.gen").getLines().mkString("\n")
      actual mustBe expected
    }
    "generate enum for JLong items" in {
      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
      val enum = JSEnum(annotation = someAnn, List[JValue](
        JLong(123), JLong(0), JLong(-456)
      ))
      val actual = treeToString(gen.generateDefinition("MyEnum", enum))
      val expected = io.Source.fromResource("enum/long_def.gen").getLines().mkString("\n")
      actual mustBe expected
    }
    "generate enum for JDecimal items" in {
      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
      val enum = JSEnum(annotation = someAnn, List[JValue](
        JDecimal(1.23), JDecimal(-4.5), JDecimal(0.6)
      ))
      val actual = treeToString(gen.generateDefinition("MyEnum", enum))
      val expected = io.Source.fromResource("enum/decimal_def.gen").getLines().mkString("\n")
      actual mustBe expected
    }
    "generate enum for JDouble items" in {
      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
      val enum = JSEnum(annotation = someAnn, List[JValue](
        JDouble(1.23), JDouble(-4.5), JDouble(0.6)
      ))
      val actual = treeToString(gen.generateDefinition("MyEnum", enum))
      val expected = io.Source.fromResource("enum/double_def.gen").getLines().mkString("\n")
      actual mustBe expected
    }
    "generate enum for JValue items" in {
      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
      val enum = JSEnum(annotation = someAnn, List[JValue](
        JString("Hello"),
        JInt(123),
        JLong(456L),
        JDecimal(4.567),
        JDouble(456.7),
        JNull,
        JBool(true),
        JsonUtils.parseJson("""[1, 2, {"a": false}]"""),
        JsonUtils.parseJson("""{"x": [true, 2]}""")
      ))
      val actual = treeToString(gen.generateDefinition("MyEnum", enum))
      val expected = io.Source.fromResource("enum/jvalue_def.gen").getLines().mkString("\n")
      actual mustBe expected
    }
  }



  //
  //  import SimpleValueHolders._
  //
  //  "StringValueHolder" should {
  //    "render enum constants" in
  //      forAll(Gen.asciiPrintableStr) { str =>
  //        whenever(!str.isEmpty) {
  //          val holder = StringValueHolder("MyEnum", JString(str))
  //          val actual: Tree = holder.renderEnumConstant
  //          val expected: Tree = CASEOBJECTDEF(StringUtils.toEnumEntryName(str))
  //            .withParents(REF("MyEnum") APPLY LIT(str))
  //          treeToString(actual) mustBe treeToString(expected)
  //        }
  //      }
  //
  //    "render resolver case" in
  //      forAll(Gen.asciiPrintableStr) { str =>
  //        whenever(!str.isEmpty) {
  //          val holder = StringValueHolder("MyEnum", JString(str))
  //          val actual: Tree = holder.renderResolverCase
  //          val entryName = StringUtils.toEnumEntryName(str)
  //          val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //          treeToString(actual) mustBe treeToString(expected)
  //        }
  //      }
  //  }
  //
  //  "IntegerValueHolder" should {
  //    "render enum constants" in
  //      forAll() { n: BigInt =>
  //        val holder = IntegerValueHolder("MyEnum", JInt(n))
  //        val actual: Tree = holder.renderEnumConstant
  //        val entryName = "MyEnum_" + n.toString.replace('-', 'N')
  //        val expected: Tree = CASEOBJECTDEF(entryName)
  //          .withParents(REF("MyEnum") APPLY (REF(BigIntClass) APPLY LIT(n.toString)))
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //
  //    "render resolver case" in
  //      forAll() { n: BigInt =>
  //        val holder = IntegerValueHolder("MyEnum", JInt(n))
  //        val actual: Tree = holder.renderResolverCase
  //        val entryName = "MyEnum_" + n.toString.replace('-', 'N')
  //        val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //  }
  //
  //  "DecimalValueHolder" should {
  //    "render enum constants" in
  //      forAll() { n: BigDecimal =>
  //        val holder = DecimalValueHolder("MyEnum", JDecimal(n))
  //        val actual: Tree = holder.renderEnumConstant
  //        val entryName = "MyEnum_" + n.underlying.toPlainString
  //          .replace('.', '_').replace('-', 'N')
  //        val expected: Tree = CASEOBJECTDEF(entryName)
  //          .withParents(REF("MyEnum") APPLY (REF(BigDecimalClass) APPLY LIT(n.toString)))
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //
  //    "render resolver case" in
  //      forAll() { n: BigDecimal =>
  //        val holder = DecimalValueHolder("MyEnum", JDecimal(n))
  //        val actual: Tree = holder.renderResolverCase
  //        val entryName = "MyEnum_" + n.underlying.toPlainString
  //          .replace('.', '_').replace('-', 'N')
  //        val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //  }
  //
  //  import JsonValueHolders._
  //
  //  "JStringValueHolder" should {
  //    "render enum constants" in
  //      forAll(Gen.asciiPrintableStr) { str =>
  //        whenever(!str.isEmpty) {
  //          val holder = JStringValueHolder("MyEnum", 2, JString(str))
  //          val actual: Tree = holder.renderEnumConstant
  //          val expected: Tree = CASEOBJECTDEF("MyEnum_2")
  //            .withParents(REF("MyEnum") APPLY (REF(Symbols.jstring) APPLY LIT(str)))
  //          treeToString(actual) mustBe treeToString(expected)
  //        }
  //      }
  //
  //    "render resolver case" in
  //      forAll(Gen.asciiPrintableStr) { str =>
  //        whenever(!str.isEmpty) {
  //          val holder = JStringValueHolder("MyEnum", 2, JString(str))
  //          val actual: Tree = holder.renderResolverCase
  //          val entryName = "MyEnum_2"
  //          val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //          treeToString(actual) mustBe treeToString(expected)
  //        }
  //      }
  //  }
  //
  //  "JIntegerValueHolder" should {
  //    "render enum constants" in
  //      forAll() { n: BigInt =>
  //        val holder = JIntegerValueHolder("MyEnum", 1, JInt(n))
  //        val actual: Tree = holder.renderEnumConstant
  //        val entryName = "MyEnum_1"
  //        val expected: Tree = CASEOBJECTDEF(entryName)
  //          .withParents(REF("MyEnum") APPLY (REF(Symbols.jint) APPLY (REF(BigIntClass) APPLY LIT(n.toString))))
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //
  //    "render resolver case" in
  //      forAll() { n: BigInt =>
  //        val holder = JIntegerValueHolder("MyEnum", 1, JInt(n))
  //        val actual: Tree = holder.renderResolverCase
  //        val entryName = "MyEnum_1"
  //        val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //  }
  //
  //  "JDecimalValueHolder" should {
  //    "render enum constants" in
  //      forAll() { n: BigDecimal =>
  //        val holder = JDecimalValueHolder("MyEnum", 3, JDecimal(n))
  //        val actual: Tree = holder.renderEnumConstant
  //        val entryName = "MyEnum_3"
  //        val expected: Tree = CASEOBJECTDEF(entryName)
  //          .withParents(REF("MyEnum") APPLY (REF(Symbols.jdecimal) APPLY (REF(BigDecimalClass) APPLY LIT(n.toString))))
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //
  //    "render resolver case" in
  //      forAll() { n: BigDecimal =>
  //        val holder = JIntegerValueHolder("MyEnum", 3, JDecimal(n))
  //        val actual: Tree = holder.renderResolverCase
  //        val entryName = "MyEnum_3"
  //        val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //        treeToString(actual) mustBe treeToString(expected)
  //      }
  //  }
  //
  //  "JNullValueHolder" should {
  //    "render enum constants" in {
  //      val holder = JNullValueHolder("MyEnum", 4)
  //      val actual: Tree = holder.renderEnumConstant
  //      val entryName = "MyEnum_4"
  //      val expected: Tree = CASEOBJECTDEF(entryName).withParents(REF("MyEnum") APPLY REF(Symbols.jnull))
  //      treeToString(actual) mustBe treeToString(expected)
  //    }
  //    "render resolver case" in {
  //      val holder = JNullValueHolder("MyEnum", 4)
  //      val actual: Tree = holder.renderResolverCase
  //      val entryName = "MyEnum_4"
  //      val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //      treeToString(actual) mustBe treeToString(expected)
  //    }
  //  }
  //
  //  "JBoolValueHolder" should {
  //    "render enum constants" in {
  //      val holder = JBoolValueHolder("MyEnum", 5, JBool(true))
  //      val actual: Tree = holder.renderEnumConstant
  //      val entryName = "MyEnum_5"
  //      val expected: Tree = CASEOBJECTDEF(entryName)
  //        .withParents(REF("MyEnum") APPLY (REF(Symbols.jbool) APPLY LIT(true)))
  //      treeToString(actual) mustBe treeToString(expected)
  //    }
  //    "render resolver case" in {
  //      val holder = JBoolValueHolder("MyEnum", 5, JBool(true))
  //      val actual: Tree = holder.renderResolverCase
  //      val entryName = "MyEnum_5"
  //      val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //      treeToString(actual) mustBe treeToString(expected)
  //    }
  //  }
  //
  //  "JCompoundValueHolder" should {
  //    "render enum constants" in {
  //      val json = JsonUtils.parseJson(
  //        """
  //          |{
  //          |  "a": true,
  //          |  "b": [1, "xyz", {"c": "d", "e": [false, null, "abc"]}],
  //          |  "x": 333
  //          |}
  //          |""".stripMargin)
  //      val holder = JCompoundValueHolder("MyEnum", 0, json)
  //      val actual: Tree = holder.renderEnumConstant
  //      val entryName = "MyEnum_0"
  //      val expected: Tree = CASEOBJECTDEF(entryName)
  //        .withParents(REF("MyEnum") APPLY (
  //          REF(Symbols.jsonMethods) DOT "parse" APPLY LIT(JsonUtils.renderJson(json)))
  //        )
  //      treeToString(actual) mustBe treeToString(expected)
  //    }
  //    "render resolver case" in {
  //      val holder = JCompoundValueHolder("MyEnum", 0, JObject())
  //      val actual: Tree = holder.renderResolverCase
  //      val entryName = "MyEnum_0"
  //      val expected: Tree = CASE(REF(entryName) DOT "value") ==> REF(entryName)
  //      treeToString(actual) mustBe treeToString(expected)
  //    }
  //  }
  //
  //  "SimpleResolverBuilder" should {
  //    "render string resolver" in {
  //      val builder = SimpleResolverBuilder("MyEnum", StringClass,
  //        List(StringValueHolder("MyEnum", JString("abc")), StringValueHolder("MyEnum", JString("xyz"))))
  //      val resolver = builder.renderResolver
  //      treeToString(resolver) mustBe treeToString(
  //        DEF("withValue", "MyEnum").withParams(PARAM("x", StringClass))
  //          .withAnnots(ANNOT("SuppressWarnings", ArrayClass APPLY LIT("org.wartremover.warts.Throw"))) :=
  //          REF("x") MATCH(
  //            CASE(REF("Abc") DOT "value") ==> REF("Abc"),
  //            CASE(REF("Xyz") DOT "value") ==> REF("Xyz"),
  //            CASE(WILDCARD) ==> THROW(IllegalArgumentExceptionClass, REF("x"))
  //          )
  //      )
  //    }
  //    "render integer resolver" in {
  //      val builder = SimpleResolverBuilder("MyEnum", BigIntClass,
  //        List(IntegerValueHolder("MyEnum", JInt(123)), IntegerValueHolder("MyEnum", JInt(456))))
  //      val resolver = builder.renderResolver
  //      treeToString(resolver) mustBe treeToString(
  //        DEF("withValue", "MyEnum").withParams(PARAM("x", BigIntClass))
  //          .withAnnots(ANNOT("SuppressWarnings", ArrayClass APPLY LIT("org.wartremover.warts.Throw"))) :=
  //          REF("x") MATCH(
  //            CASE(REF("MyEnum_123") DOT "value") ==> REF("MyEnum_123"),
  //            CASE(REF("MyEnum_456") DOT "value") ==> REF("MyEnum_456"),
  //            CASE(WILDCARD) ==> THROW(IllegalArgumentExceptionClass, REF("x"))
  //          )
  //      )
  //    }
  //    "render decimal resolver" in {
  //      val builder = SimpleResolverBuilder("MyEnum", BigDecimalClass,
  //        List(DecimalValueHolder("MyEnum", JDecimal(12.3)), DecimalValueHolder("MyEnum", JDecimal(4.56))))
  //      val resolver = builder.renderResolver
  //      treeToString(resolver) mustBe treeToString(
  //        DEF("withValue", "MyEnum").withParams(PARAM("x", BigDecimalClass))
  //          .withAnnots(ANNOT("SuppressWarnings", ArrayClass APPLY LIT("org.wartremover.warts.Throw"))) :=
  //          REF("x") MATCH(
  //            CASE(REF("MyEnum_12_3") DOT "value") ==> REF("MyEnum_12_3"),
  //            CASE(REF("MyEnum_4_56") DOT "value") ==> REF("MyEnum_4_56"),
  //            CASE(WILDCARD) ==> THROW(IllegalArgumentExceptionClass, REF("x"))
  //          )
  //      )
  //    }
  //  }


  //  import EnumGeneratorSpec._
  //
  //  "int typed enum" should {
  //    "find constant by value" in {
  //      IntEnum.withValue(1) mustBe IntEnum.IntEnum_1
  //    }
  //    "fail when resolving invalid constant" in {
  //      an[NoSuchElementException] must be thrownBy IntEnum.withValue(5)
  //    }
  //    "serialize into JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + IntEnum.serializer
  //      val json = JsonUtils.decomposeJson(IntEnum.IntEnum_2)
  //      json mustBe JInt(2)
  //    }
  //    "deserialize from JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + IntEnum.serializer
  //      val json = JInt(3)
  //      val data = JsonUtils.extractJson[IntEnum](json)
  //      data mustBe IntEnum.IntEnum_3
  //    }
  //    "fail to deserialize from invalid JSON data" in {
  //      implicit val fmt: Formats = JsonUtils.formats + IntEnum.serializer
  //      val json = JInt(5)
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[IntEnum](json)
  //    }
  //    "fail to deserialize from invalid JSON type" in {
  //      implicit val fmt: Formats = JsonUtils.formats + IntEnum.serializer
  //      val json = JString("abc")
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[IntEnum](json)
  //    }
  //  }
  //
  //  "decimal typed enum" should {
  //    "find constant by value" in {
  //      DecEnum.withValue(1.5) mustBe DecEnum.DecEnum_1
  //    }
  //    "fail when resolving invalid constant" in {
  //      an[NoSuchElementException] must be thrownBy DecEnum.withValue(5.5)
  //    }
  //    "serialize into JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + DecEnum.serializer
  //      val json = JsonUtils.decomposeJson(DecEnum.DecEnum_2)
  //      json mustBe JDecimal(2.0)
  //    }
  //    "deserialize from JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + DecEnum.serializer
  //      val json = JDecimal(3.3)
  //      val data = JsonUtils.extractJson[DecEnum](json)
  //      data mustBe DecEnum.DecEnum_3
  //    }
  //    "fail to deserialize from invalid JSON data" in {
  //      implicit val fmt: Formats = JsonUtils.formats + DecEnum.serializer
  //      val json = JDecimal(5.5)
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[DecEnum](json)
  //    }
  //    "fail to deserialize from invalid JSON type" in {
  //      implicit val fmt: Formats = JsonUtils.formats + DecEnum.serializer
  //      val json = JString("abc")
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[DecEnum](json)
  //    }
  //  }
  //
  //  "string typed enum" should {
  //    "find constant by value" in {
  //      StrEnum.withValue("SOUTH") mustBe StrEnum.South
  //    }
  //    "fail when resolving invalid constant" in {
  //      an[NoSuchElementException] must be thrownBy StrEnum.withValue("North-West")
  //    }
  //    "serialize into JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + StrEnum.serializer
  //      val json = JsonUtils.decomposeJson(StrEnum.East)
  //      json mustBe JString("EAST")
  //    }
  //    "deserialize from JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + StrEnum.serializer
  //      val json = JString("NORTH")
  //      val data = JsonUtils.extractJson[StrEnum](json)
  //      data mustBe StrEnum.North
  //    }
  //    "fail to deserialize from invalid JSON data" in {
  //      implicit val fmt: Formats = JsonUtils.formats + StrEnum.serializer
  //      val json = JString("NORTH-EAST")
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[StrEnum](json)
  //    }
  //    "fail to deserialize from invalid JSON type" in {
  //      implicit val fmt: Formats = JsonUtils.formats + StrEnum.serializer
  //      val json = JInt(3)
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[StrEnum](json)
  //    }
  //  }
  //
  //  "JValue enum" should {
  //    "find constant by value" in {
  //      JVEnum.withValue(JString("Hello")) mustBe JVEnum.JVEnum_1
  //      JVEnum.withValue(JInt(123)) mustBe JVEnum.JVEnum_2
  //      JVEnum.withValue(JObject("x" -> JArray(List(JBool(true), JInt(2))))) mustBe JVEnum.JVEnum_3
  //    }
  //    "fail when resolving invalid constant" in {
  //      an[NoSuchElementException] must be thrownBy JVEnum.withValue(JString("world"))
  //    }
  //    "serialize into JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + JVEnum.serializer
  //      JsonUtils.decomposeJson(JVEnum.JVEnum_1) mustBe JString("Hello")
  //      JsonUtils.decomposeJson(JVEnum.JVEnum_2) mustBe JInt(123)
  //      JsonUtils.decomposeJson(JVEnum.JVEnum_3) mustBe JObject("x" -> JArray(List(JBool(true), JInt(2))))
  //    }
  //    "deserialize from JSON" in {
  //      implicit val fmt: Formats = JsonUtils.formats + JVEnum.serializer
  //      JsonUtils.extractJson[JVEnum](JString("Hello")) mustBe JVEnum.JVEnum_1
  //      JsonUtils.extractJson[JVEnum](JInt(123)) mustBe JVEnum.JVEnum_2
  //      JsonUtils.extractJson[JVEnum](JObject("x" -> JArray(List(JBool(true), JInt(2))))) mustBe JVEnum.JVEnum_3
  //    }
  //    "fail to deserialize from invalid JSON data" in {
  //      implicit val fmt: Formats = JsonUtils.formats + JVEnum.serializer
  //      val json = JString("NORTH-EAST")
  //      a[MappingException] must be thrownBy JsonUtils.extractJson[JVEnum](json)
  //    }
  //  }
  //
  //  "EnumGenerator" should {
  //    "work for strings" in {
  //      val dataType = JSEnum(
  //        values = List[JValue](JString("NORTH WEST"), JString("SOUTH EAST"))
  //      )
  //      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
  //      val tree = gen.renderDefinition("StrEnum", dataType)
  //      println(treeToString(tree))
  //    }
  //    "work for ints" in {
  //      val dataType = JSEnum(
  //        annotation = Some(Annotation(title = Some("IntEnum"), description = Some("Enumerated integers."))),
  //        values = List[JValue](JInt(111), JInt(222), JInt(333))
  //      )
  //      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
  //      val tree = gen.renderDefinition("IntEnum", dataType)
  //      println(treeToString(tree))
  //    }
  //    "work for decimals" in {
  //      val dataType = JSEnum(
  //        annotation = Some(Annotation(title = Some("DecEnum"), description = Some("Enumerated decimals."))),
  //        values = List[JValue](JDecimal(1.5), JDecimal(2), JDecimal(3.3))
  //      )
  //      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
  //      val tree = gen.renderDefinition("DecEnum", dataType)
  //      println(treeToString(tree))
  //    }
  //    "work for mixed" in {
  //      val dataType = JSEnum(
  //        values = List[JValue](
  //          JString("Hello"),
  //          JInt(123),
  //          JDecimal(45.67),
  //          JNull,
  //          JBool(true),
  //          JsonUtils.parseJson("""[1, 2, {"a": false}]"""),
  //          JsonUtils.parseJson("""{"x": [true, 2]}"""))
  //      )
  //      val gen = new EnumGenerator(GeneratorConfig("com.sample"))
  //      val tree = gen.renderDefinition("JVEnum", dataType)
  //      println(treeToString(tree))
  //    }
  //  }
}

//object EnumGeneratorSpec {
//
//  sealed abstract class IntEnum(val value: BigInt)
//
//  object IntEnum {
//    case object IntEnum_1 extends IntEnum(BigInt("1"))
//    case object IntEnum_2 extends IntEnum(BigInt("2"))
//    case object IntEnum_3 extends IntEnum(BigInt("3"))
//
//    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
//    def withValue(n: BigInt): IntEnum = n match {
//      case IntEnum_1.value => IntEnum_1
//      case IntEnum_2.value => IntEnum_2
//      case IntEnum_3.value => IntEnum_3
//      case _               => throw new NoSuchElementException(n.toString)
//    }
//
//    val serializer = new CustomSerializer[IntEnum](_ => ( {
//      case JInt(n) => withValue(n)
//    }, {
//      case data: IntEnum => JInt(data.value)
//    }))
//  }
//
//  sealed abstract class DecEnum(val value: BigDecimal)
//
//  object DecEnum {
//    case object DecEnum_1 extends DecEnum(1.5)
//    case object DecEnum_2 extends DecEnum(2.0)
//    case object DecEnum_3 extends DecEnum(3.3)
//    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
//    def withValue(n: BigDecimal): DecEnum = n match {
//      case DecEnum_1.value => DecEnum_1
//      case DecEnum_2.value => DecEnum_2
//      case DecEnum_3.value => DecEnum_3
//      case _               => throw new NoSuchElementException(n.toString)
//    }
//
//    val serializer = new CustomSerializer[DecEnum](_ => ( {
//      case JDecimal(n) => withValue(n)
//    }, {
//      case data: DecEnum => JDecimal(data.value)
//    }))
//  }
//
//  sealed abstract class StrEnum(val value: String)
//  object StrEnum {
//    case object North extends StrEnum("NORTH")
//    case object South extends StrEnum("SOUTH")
//    case object East extends StrEnum("EAST")
//    case object West extends StrEnum("WEST")
//
//    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
//    def withValue(s: String): StrEnum = s match {
//      case North.value => North
//      case South.value => South
//      case East.value  => East
//      case West.value  => West
//      case _           => throw new NoSuchElementException(s)
//    }
//
//    val serializer = new CustomSerializer[StrEnum](_ => ( {
//      case JString(s) => withValue(s)
//    }, {
//      case data: StrEnum => JString(data.value)
//    }))
//  }
//
//  /*
//  "enum": [
//    "Hello",
//    123,
//    null,
//    {"x": [true, 2]},
//    [1, 2, 3]
//  ]
//   */
//  sealed abstract class JVEnum(val value: JValue)
//  object JVEnum {
//    case object JVEnum_1 extends JVEnum("Hello")
//    case object JVEnum_2 extends JVEnum(123)
//    case object JVEnum_3 extends JVEnum(JsonUtils.parseJson("""{"x": [true, 2]}"""))
//    case object JVEnum_4 extends JVEnum(JNull)
//    case object JVEnum_5 extends JVEnum(List(1, 2, 3))
//
//    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
//    def withValue(jv: JValue): JVEnum = jv match {
//      case JVEnum_1.value => JVEnum_1
//      case JVEnum_2.value => JVEnum_2
//      case JVEnum_3.value => JVEnum_3
//      case _              => throw new NoSuchElementException(JsonUtils.renderJson(jv))
//    }
//
//    val serializer = new CustomSerializer[JVEnum](_ => ( {
//      case jv: JValue => withValue(jv)
//    }, {
//      case data: JVEnum => data.value
//    }))
//  }
//}
