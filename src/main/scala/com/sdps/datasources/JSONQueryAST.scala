package com.sdps.datasources

import net.liftweb.json._

object JSONQueryAST {
    def parseJValue(input: JValue): JSONQueryAST = {
        (input \ "#type", input \ "#value") match {
            case (JString("nothing"), _) =>
                JSONQueryASTNothing
            case (JString("value"), value) =>
                JSONQueryASTValue(value)
            case (JString("property"), propertyArray: JArray) =>
                val properties = propertyArray.arr map {
                  case JString(s) => Left(s)
                  case JInt(i) => Right(i)
                }
                JSONQueryASTProperty(properties)
            case (JString("operation"), operandArray: JArray) =>
                val (operation, operands) = input \ "#operation" match {
                  case JString(s) => (s, operandArray.arr map parseJValue)
                  case JNothing => (operandArray.arr.head match { case JString(s) => s }, operandArray.arr.tail map parseJValue)
                }
                JSONQueryASTOperation(operation, operands)
            case _ =>
                JSONQueryASTValue(input)
        }
    }
}

class JSONQueryAST

case object JSONQueryASTNothing extends JSONQueryAST

case class JSONQueryASTValue(value: JValue) extends JSONQueryAST

case class JSONQueryASTProperty(property: Seq[Either[String, BigInt]]) extends JSONQueryAST

case class JSONQueryASTOperation(operation: String, operands: Seq[JSONQueryAST]) extends JSONQueryAST
