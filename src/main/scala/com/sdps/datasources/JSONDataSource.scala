package com.sdps.datasources

import scala.io.Source.fromFile
import net.liftweb.json._
import net.liftweb.json.Implicits._
import net.liftweb.json.JsonParser.ParseException
import java.util.{NoSuchElementException, UUID}
import java.io.{FileNotFoundException, File, FileWriter}
import java.util.concurrent.locks.{ReentrantReadWriteLock}
import java.util.concurrent.TimeUnit
import java.util.regex.Pattern

/**
 * Created by IntelliJ IDEA.
 * User: adamj
 * Date: 9/13/11
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 *
 * A Stupid JSON DataSource. Loads JSON data from a file. Very stupid :-)
 *
 */

abstract class JSONDataSource(override val connectionString: String) extends DataSource(connectionString) {

    protected def readData: JValue = JNothing

    protected def writeData(data: JValue): Unit

    def resolveProperty(value: JValue, properties: Seq[JValue] = Nil): JValue = properties.headOption.getOrElse { None } match {
        case JString(s: String) => resolveProperty(value \ s, properties.tail)
        case JInt(i: BigInt) => resolveProperty(value(i.intValue), properties.tail)
        case _ => value
    }

    def isLessThan(left: JValue, right: JValue) = (left, right) match {
        case (JString(a), JString(b)) => a < b
        case (JDouble(a), JDouble(b)) => a < b
        case (JInt(a), JInt(b)) => a < b
        case (JBool(a), JBool(b)) => a < b
        case _ => false
    }

    def isEqualTo(left: JValue, right: JValue) = (left, right) match {
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

    def isGreaterThan(left: JValue, right: JValue) = (left, right) match {
        case (JString(a), JString(b)) => a > b
        case (JDouble(a), JDouble(b)) => a > b
        case (JInt(a), JInt(b)) => a > b
        case (JBool(a), JBool(b)) => a > b
        case _ => false
    }

    def isLessThanOrEqualTo(left: JValue, right: JValue) = isEqualTo(left, right) || isLessThan(left, right)

    def isGreaterThanOrEqualTo(left: JValue, right: JValue) = isEqualTo(left, right) || isGreaterThan(left, right)

    def isIn(left: JValue, right: JValue) = (left, right) match {
        case (JString(a), JString(b)) => b contains a
        case (JDouble(a), JDouble(b)) => b.toString contains a.toString
        case (JInt(a), JInt(b)) => b.toString contains a.toString
        case (JBool(a), JBool(b)) => b.toString contains a.toString
        case (a: JValue, JArray(b)) => b contains a
        case (a: JValue, b: JValue) => b.children contains a
        case _ => false
    }

    def isLike(left: JValue, right: JValue) = (left, right) match {
        case (JString(a), JString(b)) => a matches b
        case (JObject(a), JObject(b)) => (a intersect b) == b
        case (JArray(a), JArray(b)) => (a intersect b) == b
        case _ => false
    }

    def startsWith(left: JValue, right: JValue) = (left, right) match {
        case (JString(a), JString(b)) => a startsWith b
        case (JDouble(a), JDouble(b)) => a.toString startsWith b.toString
        case (JInt(a), JInt(b)) => a.toString startsWith b.toString
        case (JArray(a), JArray(b)) => (a indexOfSlice b) == 0
        case _ => false
    }

    def endsWith(left: JValue, right: JValue) = (left, right) match {
        case (JString(a), JString(b)) => a endsWith b
        case (JDouble(a), JDouble(b)) => a.toString endsWith b.toString
        case (JInt(a), JInt(b)) => a.toString endsWith b.toString
        case (JArray(a), JArray(b)) => (a.reverse indexOfSlice b.reverse) == 0
        case _ => false
    }

    def filterItem(item: JValue, left: JValue, operator: String, right: JValue): Boolean = {
        //TODO: This code could probably be cleaned up some
        def tryResolveProperty(value: JValue, transformJNothings: Boolean = false) = value match {
            case JArray(possibleProperties) =>
                val resolvedProperty = resolveProperty(item, possibleProperties)
                if((resolvedProperty == JNothing) && transformJNothings) JNull else resolvedProperty
            case _ => value
        }
        val actualOperator = operator.toLowerCase.stripPrefix("|").stripPrefix(":").stripSuffix("|").stripSuffix(":")
        val actualLeft = if(operator startsWith ":") left else tryResolveProperty(left, operator startsWith "|")
        val actualRight = if(operator startsWith ":") right else tryResolveProperty(right, operator endsWith "|")
        if(!(List("and", "or") contains actualOperator.stripPrefix("!"))) {
            if(!(operator startsWith ":") && !(operator startsWith "|") && actualLeft == JNothing) return false
            if(!(operator endsWith ":") && !(operator endsWith "!") && actualRight == JNothing) return false
        }
        val comparisonResult = actualOperator.stripPrefix("!") match {
            case "<"    => isLessThan(actualLeft, actualRight)
            case "<="   => isLessThanOrEqualTo(actualLeft, actualRight)
            case "="   => isEqualTo(actualLeft, actualRight)
            case ">="   => isGreaterThanOrEqualTo(actualLeft, actualRight)
            case ">"    => isGreaterThan(actualLeft, actualRight)
            case "in"   => isIn(actualLeft, actualRight)
            case "contains" => isIn(actualRight, actualLeft)
            case "like" => isLike(actualLeft, actualRight)
            case "startswith" => startsWith(actualLeft, actualRight)
            case "endswith" => endsWith(actualLeft, actualRight)
            case "and" => (left, right) match { case (JArray(List(l1, JString(o1), r1)), JArray(List(l2, JString(o2), r2))) => filterItem(item, l1, o1, r1) & filterItem(item, l2, o2, r2) }
            case "or" => (left, right) match { case (JArray(List(l1, JString(o1), r1)), JArray(List(l2, JString(o2), r2))) => filterItem(item, l1, o1, r1) | filterItem(item, l2, o2, r2) }
        }
        if(actualOperator startsWith "!") !comparisonResult else comparisonResult
    }

    def filterItemContent(item: JValue, contentFilters: Seq[JArray]) = {
        if(contentFilters.isEmpty) item
        else item match {
            case objekt: JObject => new JObject(filterObjectContent(objekt, contentFilters).toList)
            case array: JArray  => new JArray(filterArrayContent(array, contentFilters).toList)
            case _ => item
        }
    }

    def convertPropertiesToString(properties: Seq[JValue]) = properties map {
        case JString(s) => s
        case JInt(i) => i.toString
    } mkString "."

    def filterObjectContent(objekt: JObject, contentFilters: Seq[JArray]) = for {
        JArray(properties) <- contentFilters
    } yield new JField(convertPropertiesToString(properties), resolveProperty(objekt, properties))

    def filterArrayContent(array: JArray, contentFilters: Seq[JArray]) = for {
        JArray(properties) <- contentFilters
    } yield resolveProperty(array, properties)

}

abstract class JSONFileDataSource(override val connectionString: String,  val maxWaitForLock: Int = 100) extends JSONDataSource(connectionString) {
    val sourceLock = new ReentrantReadWriteLock()
    val readLock = sourceLock.readLock()
    val writeLock = sourceLock.writeLock()

    override protected def readData = {
        if(!readLock.tryLock(maxWaitForLock, TimeUnit.MILLISECONDS)) throw new Exception("Unable to obtain Read Lock")
        try {
            parse(fromFile(connectionString).mkString) }
        catch  {
            case e: FileNotFoundException => JNothing
            case e: ParseException => JNothing
        } finally {
            readLock.unlock()
        }
    }

    protected def writeData(data: JValue) {
        if(!writeLock.tryLock(maxWaitForLock, TimeUnit.MILLISECONDS)) throw new Exception("Unable to obtain Read Lock")
        val writer = new FileWriter(new File(connectionString))
        try {
            writer.write(compact(render(data)))
        }
        finally {
            writer.flush()
            writer.close()
            writeLock.unlock()
        }
    }
}

abstract class JSONHTTPDataSource(override val connectionString: String) extends JSONDataSource(connectionString) {
    // TODO: Implement readData
    // TODO: Implement writeData

}

trait JSONObjectDataSource extends JSONDataSource {

    override protected def readData: JObject = super.readData match {
        case o: JObject => o
        case JNothing => new JObject(Nil)
    }

    protected def orderItems(items: Seq[(JValue, JValue)], orderBy: Seq[(JString, JArray)] = Nil) = if(orderBy.isEmpty) items else {
        /**
         * Logic for comparison:
         *
         * 1: Check that property value on e1 is less than property value on e2
         * 2: If not return value for next item
         */
        def compare(v1: JValue, v2: JValue, properties: Seq[(JString, JArray)]): Boolean = try {
            val (sortValue, JArray(property)) = properties.head
            val returnValue =
                    if(isLessThan(resolveProperty(v1, property), resolveProperty(v2, property))) true
                    else if(isEqualTo(resolveProperty(v1, property), resolveProperty(v2, property))) compare(v1, v2, properties.tail)
                    else false
            if(sortValue == "asc") returnValue
            else !returnValue
        } catch {
            case _ => isLessThan(v1, v2)
        }
        items sortWith { (v1, v2) => compare(v1._2, v2._2, orderBy) }
    }

    protected def sliceItems(items: Seq[(JValue, JValue)], itemRange: (JInt, JInt)): Seq[(JValue, JValue)] = {
        val (JInt(from), JInt(until)) = itemRange
        // Handle Negative Indexes
        if(from < 0)  sliceItems(items, (items.length + 1 + from,  itemRange._2))
        else if(until < 0) sliceItems(items, (itemRange._1, items.length + 1 + until))
        // Handle Reversing
        else if(from > until) sliceItems(items.reverse, (itemRange._2, itemRange._1))
        // Slice
        else items.slice(from.toInt, until.toInt)
    }

    def getItemsById(itemIds: Seq[JValue] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[(JString, JArray)] = Nil, itemRange: (JInt, JInt) = (0, -1)) =
        sliceItems(
            orderItems(
                (for {
                    JField(name, value: JObject) <- readData.obj
                    if itemIds.isEmpty || (itemIds contains name)
                } yield (new JString(name), filterItemContent(value, contentFilters))),
                orderBy),
            itemRange)

    // TODO: This should support targetValue being either just a value or a property resolving construct
    protected def filterItems(items: Seq[JField], filters: Seq[(JValue, JString, JValue)]): Seq[JField] = try {
        filters.head match {
            case (left, JString(operator), right) =>
                filterItems(items filter { (item: JField) => filterItem(item.value, left, operator, right) }, filters.tail)
        }
    } catch { case ex: NoSuchElementException => items }

    def getItemsByFilter(itemFilters: Seq[(JValue, JString, JValue)] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[(JString, JArray)] = Nil, itemRange: (JInt, JInt) = (0,-1)) =
        sliceItems(
            orderItems(
                (for {
                    JField(name, value: JObject) <- filterItems(readData.obj, itemFilters)
                } yield (new JString(name), filterItemContent(value, contentFilters))),
                orderBy),
            itemRange)

    def addItems(items: Seq[JValue]) = {
        val itemIds = for(item <- items) yield new JString(UUID.randomUUID.toString)
        updateItems(itemIds zip items)
        itemIds
    }

    def updateItems(items: Seq[(JValue, JValue)]) = {
        val newItems = new JObject(Nil ++ items map { case (id: JString, item: JObject) => new JField(id.s, item) })
        writeData(readData merge newItems)
    }

    def deleteItemsById(ids: Seq[JValue]) = writeData(new JObject(readData.obj filter { case JField(name, value: JObject) => !(ids contains name) }))
}

/**
 * A Stupid JSON Object DataSource
 *
 * Works with a file that contains a single JSON object. Each key on the object maps to a single JSON object.
 * The objects stored on the object do not need to be uniformly structured
 */
class JSONObjectFileDataSource(override val connectionString: String, override val maxWaitForLock: Int = 100) extends JSONFileDataSource(connectionString, maxWaitForLock) with JSONObjectDataSource

//class JSONObjectHTTPDataSource(override val connectionString: String, override val maxWaitForLock: Int = 100) extends JSONHTTPDataSource(connectionString) with JSONObjectDataSource

/*
class StupidJSONArrayDataSource(override val uri: String) extends StupidJSONDataSource(uri) {

    protected def readData = super.readData match { case a: JArray => a }

    def getItemsById(itemIds: Seq[JValue] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[JArray] = Nil, itemRange: (JInt, JInt) = (0,-1)) = for {
        JArray(List(JString(name), value: JValue)) <- readData.arr
        if itemIds.isEmpty || (itemIds contains name)
    } yield (new JString(name), filterItemContent(value, contentFilters))

    protected def filterItems(items: Seq[JValue], filters: Seq[(JArray, JString, JValue)]): Seq[JField] = filters.headOption match {
        case (JArray(properties), JString(operator), targetValue) => filterItems(items filter { (item: JField) => filterItem(item.value, properties, operator, targetValue) }, filters.tail)
        case _ => items
    }

    def getItemsByFilter(itemFilters: Seq[(JArray, JString, JValue)] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[JArray] = Nil, itemRange: (JInt, JInt) = (0,-1)) = for {
        JArray(List(JString(name), value: JValue)) <- filterItems(readData.arr, itemFilters)
    } yield (new JString(name), filterItemContent(value, contentFilters))
}
*/
