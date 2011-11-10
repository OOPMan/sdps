package com.sdps.datasources

import scala.io.Source.fromFile
import net.liftweb.json._
import net.liftweb.json.Implicits._
import net.liftweb.json.JsonParser.ParseException
import java.util.{NoSuchElementException, UUID}
import java.io.{FileNotFoundException, File, FileWriter}
import java.util.concurrent.locks.{ReentrantReadWriteLock}
import java.util.concurrent.TimeUnit

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
abstract class StupidJSONDataSource(override val connectionString: String, val maxWaitForLock:Long = 100) extends DataSource(connectionString) {
    val sourceLock = new ReentrantReadWriteLock()
    val readLock = sourceLock.readLock()
    val writeLock = sourceLock.writeLock()

    protected def readData = {
        val haveLock = readLock.tryLock(maxWaitForLock, TimeUnit.MILLISECONDS)
        if(haveLock) try {
            parse(fromFile(connectionString).mkString) }
        catch  {
            case e: FileNotFoundException => JNothing
            case e: ParseException => JNothing
        } finally {
            readLock.unlock()
        }
        else throw new Exception("Unable to obtain Read Lock")
    }

    protected def writeData(data: JValue) {
        val haveLock = writeLock.tryLock(maxWaitForLock, TimeUnit.MILLISECONDS)
        val writer = new FileWriter(new File(connectionString))
        if(haveLock) try {
            writer.write(compact(render(data)))
        }
        finally {
            writer.flush()
            writer.close()
            writeLock.unlock()
        }
        else throw new Exception("Unable to obtain Write Lock")
    }

    def resolveProperty(value: JValue, properties: Seq[JValue]): JValue = properties.headOption.getOrElse { None } match {
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

/**
 * A Stupid JSON Object DataSource
 *
 * Works with a file that contains a single JSON object. Each key on the object maps to a single JSON object.
 * The objects stored on the object do not need to be uniformly structured
 */
class StupidJSONObjectDataSource(override val connectionString: String) extends StupidJSONDataSource(connectionString) {

    override protected def readData: JObject = super.readData match {
        case o: JObject => o
        case JNothing=> new JObject(Nil)
    }

    //TODO: Implement sort and slice handling
    def getItemsById(itemIds: Seq[JValue] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[JArray] = Nil, itemRange: (JInt, JInt) = (0,-1)) = for {
        JField(name, value: JObject) <- readData.obj
        if itemIds.isEmpty || (itemIds contains name)
    } yield (new JString(name), filterItemContent(value, contentFilters))

    //TODO: This is shared with StupidJSONArrayDataSource. Find a way to factor it out into StupidJSONDataSource
    protected def filterItems(items: Seq[JField], filters: Seq[(JArray, JString, JValue)]): Seq[JField] = try {
        filters.head match {
            case (JArray(properties), JString(comparator), targetValue) => filterItems(items filter { (item: JField) => filterItem(item.value, properties, comparator, targetValue) }, filters.tail)
        }
    } catch { case ex: NoSuchElementException => items }

    //TODO: Implement sort and slice handling
    def getItemsByFilter(itemFilters: Seq[(JArray, JString, JValue)] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[JArray] = Nil, itemRange: (JInt, JInt) = (0,-1)) = for {
        JField(name, value: JObject) <- filterItems(readData.obj, itemFilters)
    } yield (new JString(name), filterItemContent(value, contentFilters))

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
/*
class StupidJSONArrayDataSource(override val uri: String) extends StupidJSONDataSource(uri) {

    protected def readData = super.readData match { case a: JArray => a }

    def getItemsById(itemIds: Seq[JValue] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[JArray] = Nil, itemRange: (JInt, JInt) = (0,-1)) = for {
        JArray(List(JString(name), value: JValue)) <- readData.arr
        if itemIds.isEmpty || (itemIds contains name)
    } yield (new JString(name), filterItemContent(value, contentFilters))

    protected def filterItems(items: Seq[JValue], filters: Seq[(JArray, JString, JValue)]): Seq[JField] = filters.headOption match {
        case (JArray(properties), JString(comparator), targetValue) => filterItems(items filter { (item: JField) => filterItem(item.value, properties, comparator, targetValue) }, filters.tail)
        case _ => items
    }

    def getItemsByFilter(itemFilters: Seq[(JArray, JString, JValue)] = Nil, contentFilters: Seq[JArray] = Nil, orderBy: Seq[JArray] = Nil, itemRange: (JInt, JInt) = (0,-1)) = for {
        JArray(List(JString(name), value: JValue)) <- filterItems(readData.arr, itemFilters)
    } yield (new JString(name), filterItemContent(value, contentFilters))
}
*/
