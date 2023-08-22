package com.uralian.jason.v7.generator

import com.uralian.jason.util.{JsonUtils, StringUtils}
import com.uralian.jason.v7.ast.JSEnum
import org.json4s._
import treehugger.forest._
import definitions._
import treehuggerDSL._

/**
 * Renders JSON serializer for enum companion object.
 */
sealed trait SerializerBuilder {

  /**
   * Renders a custom serializer for enum companion object.
   *
   * @param typeName target type name.
   * @return custom serializer for marshalling/unmarshalling values into JSON.
   */
  def render(typeName: String): Tree = {
    val lambda = LAMBDA(PARAM(WILDCARD)) ==> TUPLE(desFunc(typeName), serFunc(typeName))
    VAL("serializer") := NEW(Symbols.customSer APPLYTYPE typeName APPLY lambda)
  }

  /**
   * Renders deserializer function.
   *
   * @param typeName target type name.
   * @return AST for deserializer function.
   */
  protected def desFunc(typeName: String): Tree

  /**
   * Renders serializer function.
   *
   * @param typeName target type name.
   * @return AST for serializer function.
   */
  protected def serFunc(typeName: String): Tree
}

/**
 * Factory for [[SerializerBuilder]] instances.
 */
object SerializerBuilder {

  /**
   * Implementation of [[SerializerBuilder]] for simple enums.
   *
   * @param jsonType JSON type for enum serialization.
   */
  final case class SimpleSerializerBuilder(jsonType: Type) extends SerializerBuilder {

    protected def desFunc(typeName: String): Tree = BLOCK(
      CASE(jsonType UNAPPLY ID("x")) ==> REF("withValue") APPLY REF("x")
    )

    protected def serFunc(typeName: String): Tree = BLOCK(
      CASE(ID("data") withType typeName) ==> jsonType APPLY (REF("data") DOT "value")
    )
  }

  /**
   * Implementation of [[SerializerBuilder]] for compound enums.
   */
  final case class CompoundSerializerBuilder() extends SerializerBuilder {

    protected def desFunc(typeName: String): Tree = BLOCK(
      CASE(ID("jv") withType Symbols.jvalue) ==> REF("withValue") APPLY REF("jv")
    )

    protected def serFunc(typeName: String): Tree = BLOCK(
      CASE(ID("data") withType typeName) ==> REF("data") DOT "value"
    )
  }
}

/**
 * Renders enum resolver function for enum companion object.
 */
sealed trait ResolverBuilder {

  import ResolverBuilder._

  /**
   * Entry value type.
   *
   * @return enum entry value type (String, BigInt, Long, BigDecimal, Double or JValue).
   */
  def valueType: Type

  /**
   * Renders a resolver function that returns an entry for the supplied value.
   *
   * @param typeName   target type name.
   * @param entryNames a list of enum entry names.
   * @return AST for resolver function.
   */
  def render(typeName: String, entryNames: Seq[String]): Tree = {
    val validCases = entryNames.map(name => CASE(REF(name) DOT "value") ==> REF(name))
    val invalidCase = CASE(WILDCARD) ==> THROW(IllegalArgumentExceptionClass, invalidCaseArg)
    val allCases = validCases :+ invalidCase
    DEF("withValue", typeName)
      .withParams(PARAM("x", valueType))
      .withAnnots(annotations) :=
      REF("x") MATCH allCases
  }

  /**
   * Renders the argument for "not found" exception case.
   *
   * @return the argument supplied to the exception thrown if the entry could not be resolved.
   */
  protected def invalidCaseArg: Tree
}

/**
 * Factory for [[ResolverBuilder]] instances.
 */
object ResolverBuilder {

  /**
   * Annotations for the resolver function.
   */
  val annotations = List(
    ANNOT("SuppressWarnings", ArrayClass APPLY LIT("org.wartremover.warts.Throw"))
  )

  /**
   * Implementation of [[ResolverBuilder]] for simple enums.
   *
   * @param valueType enum value type.
   */
  final case class SimpleResolverBuilder(valueType: Type) extends ResolverBuilder {
    protected val invalidCaseArg: Tree = REF("x")
  }

  /**
   * Implementation of [[ResolverBuilder]] for compound enums.
   */
  final case class CompoundResolverBuilder() extends ResolverBuilder {
    val valueType = Symbols.jvalue

    protected def invalidCaseArg: Tree = REF(Symbols.jsonMethods) DOT "pretty" APPLY
      (REF(Symbols.jsonMethods) DOT "render" APPLY REF("x"))
  }
}

/**
 * Renders enum definition for a given pair of type parameters.
 * The following combinations of type parameters are accepted:
 *
 * {{{
 * +-------------+------------+
 * | JValue Type | Scala Type |
 * + ----------- + ---------- +
 * | JString     | String     |
 * | JInt        | BigInt     |
 * | JLong       | Long       |
 * | JDecimal    | BigDecimal |
 * | JDouble     | Double     |
 * | JValue      | JValue     |
 * +-------------+------------+
 * }}}
 *
 * @tparam JT JValue subclass carrying enum constants.
 * @tparam ST Scala class that enum values get serialized to.
 */
sealed trait EnumDefinitionBuilder[JT <: JValue, ST] {

  /**
   * Renders enum definition tree, which includes imports, abstract entry class and a companion
   * object with constant definitions, resolver and serializer function.
   *
   * @param typeName    target type name.
   * @param description optional description.
   * @param values      enum values as JSON entities.
   * @param packageName the name of the package for generated code.
   * @return AST for enum definition.
   */
  def render(typeName: String,
             description: Option[String],
             values: Seq[JT],
             packageName: String): Tree = {

    val imports = renderImports
    val entry = renderEntryDefinition(typeName, description)
    val companion = renderCompanionDefinition(typeName, values)

    BLOCK(imports :+ entry :+ companion).inPackage(packageName)
  }

  /**
   * Renders import statements.
   *
   * @return a list of import statements.
   */
  protected def renderImports = List(
    IMPORT(Symbols.json4s, "_"),
    IMPORT(Symbols.json4sMethods)
  )

  /**
   * Renders enum entry type definition.
   *
   * @param typeName    target type name.
   * @param description optional description.
   * @return entry type definition.
   */
  protected def renderEntryDefinition(typeName: String, description: Option[String]): Tree =
    CLASSDEF(typeName)
      .withFlags(Flags.SEALED, Flags.ABSTRACT)
      .withParams(VAL("value", valueType)).tree
      .withDoc(description, DocTag.Param("value", "entry value."))

  /**
   * Renders enum companion object definition.
   *
   * @param typeName target type name.
   * @param values   enum values as JSON entities.
   * @return definition of enum companion object.
   */
  protected def renderCompanionDefinition(typeName: String, values: Seq[JT]): Tree = {

    val entryNamesAndValues = values.zipWithIndex map {
      case (jv, index) => buildEntryName(typeName, index, jv) -> jv
    }

    val entries = renderEnumConstants(typeName, entryNamesAndValues)
    val resolver = resolverBuilder.render(typeName, entryNamesAndValues.map(_._1))
    val serializer = serializerBuilder.render(typeName)

    val contents = entries :+ resolver :+ serializer

    (OBJECTDEF(typeName) := BLOCK(contents)).withDoc(s"Available $typeName constants.")
  }

  /**
   * Renders enum constant definitions (`case object ...`).
   *
   * @param typeName       target type name.
   * @param namesAndValues enum entries as (name, value) pairs.
   * @return a list of enum constant definitions.
   */
  protected def renderEnumConstants(typeName: String,
                                    namesAndValues: Seq[(String, JT)]): Seq[Tree] = namesAndValues map {
    case (name, jv) => CASEOBJECTDEF(name).withParents(REF(typeName) APPLY buildEntryArg(jv))
  }

  /**
   * Entry value type.
   *
   * @return one of the following: `String`, `BigInt`, `Long`, `BigDecimal`, `Double`, `JValue`.
   */
  def valueType: Type

  protected def buildEntryName(typeName: String, index: Int, jv: JT): String

  protected def buildEntryArg(jv: JT): Tree

  protected def serializerBuilder: SerializerBuilder

  protected def resolverBuilder: ResolverBuilder
}

/**
 * Available enum builders.
 */
object EnumDefinitionBuilder {

  /**
   * Parent trait for Enum builders for simple types: strings or numbers.
   *
   * @tparam JT JValue subclass carrying enum constants.
   * @tparam ST Scala class that enum values get serialized to.
   */
  sealed trait SimpleEnumBuilder[JT <: JValue, ST] extends EnumDefinitionBuilder[JT, ST] {

    /**
     * JSON value type.
     *
     * @return one of the following: `JString`, `JInt`, `JLong`, `JDecimal`, `JDouble`, `JValue`.
     */
    def jsonType: Type

    protected val serializerBuilder = SerializerBuilder.SimpleSerializerBuilder(jsonType)

    protected val resolverBuilder = ResolverBuilder.SimpleResolverBuilder(valueType)

    protected def sanitizeNumString(s: String) = s
      .replace('.', '_')
      .replace('-', 'N')
      .replace('+', 'P')
  }

  /**
   * Builder for String/JString-based enums.
   */
  implicit object StringEnumBuilder extends SimpleEnumBuilder[JString, String] {
    def valueType = StringClass

    def jsonType = Symbols.jstring

    private val idCounter = collection.mutable.Map.empty[String, Int]

    protected def buildEntryName(typeName: String, index: Int, jv: JString): String = this.synchronized {
      val idBase = StringUtils.toEnumEntryName(jv.s)
      val count = idCounter.getOrElse(idBase, 0)
      idCounter.update(idBase, count + 1)
      if (count > 0)
        s"$idBase${count.toString}"
      else
        s"$idBase"
    }

    protected def buildEntryArg(jv: JString): Tree = LIT(jv.s)
  }

  /**
   * Builder for BigInt/JInt-based enums.
   */
  implicit object IntEnumBuilder extends SimpleEnumBuilder[JInt, BigInt] {
    def valueType = BigIntClass

    def jsonType = Symbols.jint

    protected def buildEntryName(typeName: String, index: Int, jv: JInt): String =
      typeName + "_" + sanitizeNumString(jv.num.underlying.toString)

    protected def buildEntryArg(jv: JInt): Tree = valueType APPLY LIT(JsonUtils.renderJson(jv))
  }

  /**
   * Builder for Long/JLong-based enums.
   */
  implicit object LongEnumBuilder extends SimpleEnumBuilder[JLong, Long] {
    def valueType = LongClass

    def jsonType = Symbols.jlong

    protected def buildEntryName(typeName: String, index: Int, jv: JLong): String =
      typeName + "_" + sanitizeNumString(jv.num.toString)

    protected def buildEntryArg(jv: JLong): Tree = valueType APPLY LIT(jv.num)
  }

  /**
   * Builder for BigDecimal/JDecimal-based enums.
   */
  implicit object DecimalEnumBuilder extends SimpleEnumBuilder[JDecimal, BigDecimal] {
    def valueType = BigDecimalClass

    def jsonType = Symbols.jdecimal

    protected def buildEntryName(typeName: String, index: Int, jv: JDecimal): String =
      typeName + "_" + sanitizeNumString(jv.num.underlying.toPlainString)

    protected def buildEntryArg(jv: JDecimal): Tree = valueType APPLY LIT(JsonUtils.renderJson(jv))
  }

  /**
   * Builder for Double/JDouble-based enums.
   */
  implicit object DoubleEnumBuilder extends SimpleEnumBuilder[JDouble, Double] {
    def valueType = DoubleClass

    def jsonType = Symbols.jdouble

    protected def buildEntryName(typeName: String, index: Int, jv: JDouble): String =
      typeName + "_" + sanitizeNumString(BigDecimal(jv.num).underlying.toPlainString)

    protected def buildEntryArg(jv: JDouble): Tree = valueType APPLY LIT(jv.num)
  }

  /**
   * Builder for JValue-based enums.
   */
  implicit object JValueEnumBuilder extends EnumDefinitionBuilder[JValue, JValue] {
    def valueType = Symbols.jvalue

    protected val serializerBuilder = SerializerBuilder.CompoundSerializerBuilder()

    protected val resolverBuilder = ResolverBuilder.CompoundResolverBuilder()

    protected def buildEntryName(typeName: String, index: Int, jv: JValue): String =
      s"${typeName}_${index.toString}"

    protected def buildEntryArg(jv: JValue): Tree = jv match {
      case JString(s)  => REF(Symbols.jstring) APPLY LIT(s)
      case JInt(n)     => REF(Symbols.jint) APPLY (REF(BigIntClass) APPLY LIT(n.toString))
      case JLong(n)    => REF(Symbols.jlong) APPLY LIT(n)
      case JDecimal(n) => REF(Symbols.jdecimal) APPLY (REF(BigDecimalClass) APPLY LIT(n.toString))
      case JDouble(n)  => REF(Symbols.jdouble) APPLY LIT(n)
      case JNull       => REF(Symbols.jnull)
      case JBool(b)    => REF(Symbols.jbool) APPLY LIT(b)
      case _           => REF(Symbols.jsonMethods) DOT "parse" APPLY LIT(JsonUtils.renderJson(jv))
    }
  }
}

/**
 * Generates an enum definition from [[JSEnum]] type.
 *
 * @param config generator configuration.
 */
class EnumGenerator(config: GeneratorConfig) {

  import EnumDefinitionBuilder._

  /**
   * Generates an enum definition for a given type name and enum AST.
   *
   * @param typeName target type name.
   * @param enum     source enum.
   * @return AST for enum definition.
   */
  def generateDefinition(typeName: String, enum: JSEnum): Tree = {
    val valueClasses = enum.values.map(_.getClass).distinct
    val valueType = enum.values.headOption.filter(_ => valueClasses.size == 1).getOrElse(JObject())
    valueType match {
      case _: JString  => generate[JString, String](typeName, enum)
      case _: JInt     => generate[JInt, BigInt](typeName, enum)
      case _: JLong    => generate[JLong, Long](typeName, enum)
      case _: JDecimal => generate[JDecimal, BigDecimal](typeName, enum)
      case _: JDouble  => generate[JDouble, Double](typeName, enum)
      case _           => generate[JValue, JValue](typeName, enum)
    }
  }

  private def generate[JT <: JValue, ST](typeName: String, enum: JSEnum)
                                        (implicit edb: EnumDefinitionBuilder[JT, ST]) = {
    val values = enum.values.asInstanceOf[Seq[JT]]
    edb.render(typeName, enum.annotation.flatMap(_.description), values, config.packageName)
  }
}
