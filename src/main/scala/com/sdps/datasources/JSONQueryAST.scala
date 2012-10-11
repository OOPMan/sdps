package com.sdps.datasources

import net.liftweb.json._

object JSONQueryAST {

    abstract class JSONQueryASTValue

    case object JSONQueryASTNothing extends JSONQueryASTValue

    case object JSONQueryASTNull extends JSONQueryASTValue

    case class JSONQueryASTString(s: String) extends JSONQueryASTValue

    case class JSONQueryASTDouble(num: Double) extends JSONQueryASTValue

    case class JSONQueryASTInt(num: BigInt) extends JSONQueryASTValue

    case class JSONQueryASTBool(value: Boolean) extends JSONQueryASTValue

    case class JSONQueryASTField(name: String, value: JSONQueryASTValue) extends JSONQueryASTValue

    case class JSONQueryASTObject(obj: List[JSONQueryASTField]) extends JSONQueryASTValue

    case class JSONQueryASTArray(arr: List[JSONQueryASTValue]) extends JSONQueryASTValue

    case class JSONQueryASTProperty(property: List[Either[String, BigInt]]) extends JSONQueryASTValue

    case class JSONQueryASTOperation(operation: String, operands: List[JSONQueryASTValue]) extends JSONQueryASTValue

    def parseJValue(input: JValue): JSONQueryASTValue = (input, input \ "#type", input \ "#value") match {
        case (input, JString("value"), value) =>
            parseJValue(value)
        case (input, JString("property"), propertyArray: JArray) =>
            val properties = propertyArray.arr map {
                case JString(s) => Left(s)
                case JInt(i) => Right(i)
            }
            JSONQueryASTProperty(properties)
        case (input, JString("operation"), operandArray: JArray) =>
            val (operation, operands) = input \ "#operation" match {
                case JString(s) => (s, operandArray.arr map parseJValue)
                case _ => (operandArray.arr.head match {
                    case JString(s) => s
                }, operandArray.arr.tail map parseJValue)
            }
            JSONQueryASTOperation(operation, operands)
        case (JNothing, _, _) => JSONQueryASTNothing
        case (JNull, _, _) => JSONQueryASTNull
        case (JString(s), _, _) => JSONQueryASTString(s)
        case (JDouble(d), _, _) => JSONQueryASTDouble(d)
        case (JInt(i), _, _) => JSONQueryASTInt(i)
        case (JBool(b), _, _) => JSONQueryASTBool(b)
        case (JField(k, v), _, _) => JSONQueryASTField(k, parseJValue(v))
        case (JObject(o), _, _) => JSONQueryASTObject(o map { case JField(k, v) => JSONQueryASTField(k, parseJValue(v)) })
        case (JArray(a), _, _) => JSONQueryASTArray(a map parseJValue)
    }
}


