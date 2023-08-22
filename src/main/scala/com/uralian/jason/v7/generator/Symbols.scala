package com.uralian.jason.v7.generator

import treehugger.forest._
import definitions._

/**
 * Symbols used by tree generators.
 */
object Symbols {

  /* JSON */
  val json4s = RootClass.newPackage("org.json4s")
  val json4sMethods = RootClass.newPackage("org.json4s.native.JsonMethods")
  val jvalue = RootClass.newClass("JValue")
  val jstring = RootClass.newClass("JString")
  val jint = RootClass.newClass("JInt")
  val jlong = RootClass.newClass("JLong")
  val jdecimal = RootClass.newClass("JDecimal")
  val jdouble = RootClass.newClass("JDouble")
  val jnull = RootClass.newClass("JNull")
  val jbool = RootClass.newClass("JBool")
  val jarray = RootClass.newClass("JArray")
  val jsonMethods = RootClass.newClass("JsonMethods")
  val customSer = RootClass.newClass("CustomSerializer")
}
