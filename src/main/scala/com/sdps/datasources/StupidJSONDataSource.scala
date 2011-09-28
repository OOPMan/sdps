package com.sdps.datasources

import scala.io.Source.fromURI
import net.liftweb.json._
import java.net.URI
import java.io.{File, FileWriter}
import java.util.UUID
import net.liftweb.json.Implicits._

/**
 * Created by IntelliJ IDEA.
 * User: adamj
 * Date: 9/13/11
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 *
 * A Stupid JSON DataSource. Loads JSON data from a file. Very stupid :-)
 *
 * {
 *  "item1": { "a": "value", "b": 0 }
 *  "item2": { "a": "value", "b": 0 }
 * }
 */
abstract class StupidJSONDataSource(override val uri: URI) extends DataSource(uri) {

    protected def readData = parse(fromURI(uri).mkString)

    protected def writeData(data: JValue) {
        val writer = new FileWriter(new File(uri))
        try { writer.write(compact(render(data))) }
        finally { writer.close() }
    }

    def resolveProperty(value: JValue, properties: Seq[JValue]): JValue = properties.headOption match {
        case JString(s: String) => resolveProperty(value \ s, properties.tail)
        case JInt(i: BigInt) => resolveProperty(value(i.intValue), properties.tail)
        case _ => value
    }

    def isLessThan(itemValue: JValue, targetValue: JValue) = (itemValue, targetValue) match {
        case (JString(a), JString(b)) => a < b
        case (JDouble(a), JDouble(b)) => a < b
        case (JInt(a), JInt(b)) => a < b
        case (JBool(a), JBool(b)) => a < b
        case _ => false
    }

    def isEqualTo(itemValue: JValue, targetValue: JValue) = (itemValue, targetValue) match {
        case (JNothing, JNothing) => true
        case (JNull, JNull) => true
        case (JString(a), JString(b)) => a == b
        case (JDouble(a), JDouble(b)) => a == b
        case (JInt(a), JInt(b)) => a == b
        case (JBool(a), JBool(b)) => a == b
        case (a: JObject, b: JObject) => a == b
        case (a: JArray, b: JArray) => a == b
        case _ => false
    }

    def isGreaterThan(itemValue: JValue, targetValue: JValue) = (itemValue, targetValue) match {
        case (JString(a), JString(b)) => a > b
        case (JDouble(a), JDouble(b)) => a > b
        case (JInt(a), JInt(b)) => a > b
        case (JBool(a), JBool(b)) => a > b
        case _ => false
    }

    def isLessThanOrEqualTo(itemValue: JValue, targetValue: JValue) = isEqualTo(itemValue, targetValue) || isLessThan(itemValue, targetValue)

    def isGreaterThanOrEqualTo(itemValue: JValue, targetValue: JValue) = isEqualTo(itemValue, targetValue) || isGreaterThan(itemValue, targetValue)

    def isIn(itemValue: JValue, targetValue: JValue) = (itemValue, targetValue) match {
        case (JString(a), JString(b)) => a contains b
        case (JDouble(a), JDouble(b)) => a.toString contains b.toString
        case (JInt(a), JInt(b)) => a.toString contains b.toString
        case (JArray(a), b: JValue) => a contains b
        case (a: JObject, b: JValue) => a.children contains b
        case _ => false
    }

    def isLike(itemValue: JValue, targetValue: JValue) = (itemValue, targetValue) match {
        case (JString(a), JString(b)) => a matches b
        case (JObject(a), JObject(b)) => (a intersect b) == b
        case (JArray(a), JArray(b)) => (a intersect b) == b
        case _ => false
    }

    def filterItem(item: JValue, properties: Seq[JValue], comparator: String, value: JValue) = {
        val comparisonResult = comparator.stripPrefix("!") match {
            case "<"    => isLessThan(resolveProperty(item, properties), value)
            case "<="   => isLessThanOrEqualTo(resolveProperty(item, properties), value)
            case "=="   => isEqualTo(resolveProperty(item, properties), value)
            case ">="   => isGreaterThanOrEqualTo(resolveProperty(item, properties), value)
            case ">"    => isGreaterThan(resolveProperty(item, properties), value)
            case "in"   => isIn(resolveProperty(item, properties), value)
            case "like" => isLike(resolveProperty(item, properties), value)
        }
        if(comparator startsWith "!") !comparisonResult else comparisonResult
    }
}

class StupidJSONObjectDataSource(override val uri: URI) extends StupidJSONDataSource(uri) {

    protected def readData = super.readData match { case o: JObject => o }

    def getItemsById(ids: Seq[JValue] = Nil, attributes: Seq[JString] = Nil) = for {
            JField(name, value: JObject) <- readData.obj
            if (ids.length == 0) || (ids contains name)
            final_value = if(attributes.length == 0) value else new JObject(value.obj filter { case JField(fieldname, _) => attributes contains fieldname } )
        } yield final_value

    //TODO: Implement when we decide how filters will actually work!
    def getItemsByFilter(filters: Seq[(JArray, JString, JValue)] = Nil, attributes: Seq[JString] = Nil) = {
        var items = readData.obj
        null
    }

    def addItems(items: Seq[JObject]) = {
        val itemIds = for(item <- items) yield new JString(UUID.randomUUID.toString)
        updateItems(itemIds zip items)
        itemIds
    }

    def updateItems(items: Seq[(JValue, JObject)]) = {
        val newItems = new JObject(Nil ++ items map { case (id: JString, item: JObject) => new JField(id.s, item) })
        writeData(readData merge newItems)
    }

    //TODO: Implement
    def deleteItemsById(ids: Seq[JValue]) = null
}

class StupidJSONArrayDataSource(override val uri: URI) extends StupidJSONDataSource(uri) {

    protected def readData = super.readData match { case a: JArray => a }
}

