/*
 * GNU LESSER GENERAL PUBLIC LICENSE
 *
 * Version 3, 29 June 2007
 *
 * Copyright © 2007 Free Software Foundation, Inc. <http://fsf.org/>
 *
 * Everyone is permitted to copy and distribute verbatim copies of this license document, but changing it is not allowed.
 *
 * This version of the GNU Lesser General Public License incorporates the terms and conditions of version 3 of the GNU General Public License, supplemented by the additional permissions listed below.
 *
 * 0. Additional Definitions.
 * As used herein, “this License” refers to version 3 of the GNU Lesser General Public License, and the “GNU GPL” refers to version 3 of the GNU General Public License.
 *
 * “The Library” refers to a covered work governed by this License, other than an Application or a Combined Work as defined below.
 *
 * An “Application” is any work that makes use of an interface provided by the Library, but which is not otherwise based on the Library. Defining a subclass of a class defined by the Library is deemed a mode of using an interface provided by the Library.
 *
 * A “Combined Work” is a work produced by combining or linking an Application with the Library. The particular version of the Library with which the Combined Work was made is also called the “Linked Version”.
 *
 * The “Minimal Corresponding Source” for a Combined Work means the Corresponding Source for the Combined Work, excluding any source code for portions of the Combined Work that, considered in isolation, are based on the Application, and not on the Linked Version.
 *
 * The “Corresponding Application Code” for a Combined Work means the object code and/or source code for the Application, including any data and utility programs needed for reproducing the Combined Work from the Application, but excluding the System Libraries of the Combined Work.
 *
 * 1. Exception to Section 3 of the GNU GPL.
 * You may convey a covered work under sections 3 and 4 of this License without being bound by section 3 of the GNU GPL.
 *
 * 2. Conveying Modified Versions.
 * If you modify a copy of the Library, and, in your modifications, a facility refers to a function or data to be supplied by an Application that uses the facility (other than as an argument passed when the facility is invoked), then you may convey a copy of the modified version:
 *  a) under this License, provided that you make a good faith effort to ensure that, in the event an Application does not supply the function or data, the facility still operates, and performs whatever part of its purpose remains meaningful, or
 *  b) under the GNU GPL, with none of the additional permissions of this License applicable to that copy.
 *
 * 3. Object Code Incorporating Material from Library Header Files.
 * The object code form of an Application may incorporate material from a header file that is part of the Library. You may convey such object code under terms of your choice, provided that, if the incorporated material is not limited to numerical parameters, data structure layouts and accessors, or small macros, inline functions and templates (ten or fewer lines in length), you do both of the following:
 *  a) Give prominent notice with each copy of the object code that the Library is used in it and that the Library and its use are covered by this License.
 *  b) Accompany the object code with a copy of the GNU GPL and this license document.
 *
 * 4. Combined Works.
 * You may convey a Combined Work under terms of your choice that, taken together, effectively do not restrict modification of the portions of the Library contained in the Combined Work and reverse engineering for debugging such modifications, if you also do each of the following:
 *  a) Give prominent notice with each copy of the Combined Work that the Library is used in it and that the Library and its use are covered by this License.
 *  b) Accompany the Combined Work with a copy of the GNU GPL and this license document.
 *  c) For a Combined Work that displays copyright notices during execution, include the copyright notice for the Library among these notices, as well as a reference directing the user to the copies of the GNU GPL and this license document.
 *  d) Do one of the following:
 *   0) Convey the Minimal Corresponding Source under the terms of this License, and the Corresponding Application Code in a form suitable for, and under terms that permit, the user to recombine or relink the Application with a modified version of the Linked Version to produce a modified Combined Work, in the manner specified by section 6 of the GNU GPL for conveying Corresponding Source.
 *   1) Use a suitable shared library mechanism for linking with the Library. A suitable mechanism is one that (a) uses at run time a copy of the Library already present on the user's computer system, and (b) will operate properly with a modified version of the Library that is interface-compatible with the Linked Version.
 *  e) Provide Installation Information, but only if you would otherwise be required to provide such information under section 6 of the GNU GPL, and only to the extent that such information is necessary to install and execute a modified version of the Combined Work produced by recombining or relinking the Application with a modified version of the Linked Version. (If you use option 4d0, the Installation Information must accompany the Minimal Corresponding Source and Corresponding Application Code. If you use option 4d1, you must provide the Installation Information in the manner specified by section 6 of the GNU GPL for conveying Corresponding Source.)
 *
 * 5. Combined Libraries.
 * You may place library facilities that are a work based on the Library side by side in a single library together with other library facilities that are not Applications and are not covered by this License, and convey such a combined library under terms of your choice, if you do both of the following:
 *  a) Accompany the combined library with a copy of the same work based on the Library, uncombined with any other library facilities, conveyed under the terms of this License.
 *  b) Give prominent notice with the combined library that part of it is a work based on the Library, and explaining where to find the accompanying uncombined form of the same work.
 *
 * 6. Revised Versions of the GNU Lesser General Public License.
 * The Free Software Foundation may publish revised and/or new versions of the GNU Lesser General Public License from time to time. Such new versions will be similar in spirit to the present version, but may differ in detail to address new problems or concerns.
 *
 * Each version is given a distinguishing version number. If the Library as you received it specifies that a certain numbered version of the GNU Lesser General Public License “or any later version” applies to it, you have the option of following the terms and conditions either of that published version or of any later version published by the Free Software Foundation. If the Library as you received it does not specify a version number of the GNU Lesser General Public License, you may choose any version of the GNU Lesser General Public License ever published by the Free Software Foundation.
 *
 * If the Library as you received it specifies that a proxy can decide whether future versions of the GNU Lesser General Public License shall apply, that proxy's public statement of acceptance of any version is permanent authorization for you to choose that version for the Library.
 */

package com.sdps.datasources
package json

import scala.io.Source.fromFile
import net.liftweb.json._
import net.liftweb.json.Implicits._
import net.liftweb.json.JsonParser.ParseException
import java.util.{NoSuchElementException, UUID}
import java.io.{FileNotFoundException, File, FileWriter}
import java.util.concurrent.locks.{ReentrantReadWriteLock}
import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import org.scalatra.util.RicherString
import java.util
import collection.immutable.WrappedString
import com.sdps.utils.JSONQueryAST.JSONQueryASTValue

object JSONDataSource {
    def resolveProperty(value: JValue, properties: Seq[Either[String, BigInt]] = Nil): JValue = properties.headOption.getOrElse { None } match {
        case Left(s: String) => resolveProperty(value \ s, properties.tail)
        case Right(i: BigInt) => resolveProperty(value(i.intValue), properties.tail)
        case _ => value
    }

    sealed abstract class ASTValue {
        def apply(context: JValue = JNothing): Any
    }

    case object ASTNothing extends ASTValue {
        def apply(context: JValue) = None
    }

    case object ASTNull extends ASTValue {
        def apply(context: JValue) = null
    }

    case class ASTString(s: String) extends ASTValue{
        def apply(context: JValue) = s
    }

    case class ASTDouble(num: Double) extends ASTValue {
        def apply(context: JValue) = num
    }

    case class ASTInt(num: BigInt) extends ASTValue {
        def apply(context: JValue) = num
    }

    case class ASTBool(value: Boolean) extends ASTValue {
        def apply(context: JValue) = value
    }

    case class ASTField(name: String, value: ASTValue) extends ASTValue {
        def apply(context: JValue) = (name, value)
    }

    case class ASTObject(obj: List[ASTField]) extends ASTValue {
        def apply(context: JValue) = obj
    }

    case class ASTArray(arr: List[ASTValue]) extends ASTValue {
        def apply(context: JValue) = arr
    }

    case class ASTProperty(property: List[Either[String, BigInt]]) extends ASTValue {
        def apply(context: JValue) = resolveProperty(context, property).values
    }

    sealed abstract class ASTOperation(operation: String, operands: List[ASTValue]) extends ASTValue

    sealed abstract class ComparativeASTOperation(operation: String, operands: List[ASTValue]) extends ASTOperation(operation, operands) {
        def performComparison[T](a: Ordered[T], b: T): Boolean

        def compare(a: Any, b: Any): Boolean = (a, b) match {
            case (a: String, b: String) => performComparison(a, b)
            case (a: String, b: Int) => performComparison(a, b.toString)
            case (a: String, b: Double) => performComparison(a, b.toString)
            case (a: String, b: Boolean) => performComparison(a, b.toString)
            case (a: Int, b: Int) => performComparison(a, b)
            case (a: Int, b: String) => performComparison(a, b.toInt)
            case (a: Int, b: Double) => performComparison(a, b.toInt)
            case (a: Int, b: Boolean) => performComparison(a, if(b) 1 else 0)
            case (a: Double, b: Double) => performComparison(a, b)
            case (a: Double, b: String) => performComparison(a, b.toDouble)
            case (a: Double, b: Int) => performComparison(a, b.toDouble)
            case (a: Double, b: Boolean) => performComparison(a, if(b) 1.0 else 0.0)
            case (a: Boolean, b: Boolean) => performComparison(a, b)
            case (a: Boolean, b: String) => performComparison(a, b.toBoolean)
            case (a: Boolean, b: Int) => performComparison(a, if(b == 1) true else false)
            case (a: Boolean, b: Double) => performComparison(a, if(b == 1.0) true else false)
            case _ => false
        }

        def apply(context: JValue): Boolean = operands.sliding(2).forall { case List(a, b) => compare(a(context), b(context)) }
    }

    case class LessThanASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation("<", operands) {
        def performComparison[T](a: Ordered[T], b: T) = a < b
    }

    case class LessOrEqualToASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation("<=", operands) {
        def performComparison[T](a: Ordered[T], b: T) = a <= b
    }

    trait EqualToComparison extends ComparativeASTOperation {
        def performComparison[T](a: Ordered[T], b: T) = a == b

        override def compare(a: Any, b: Any) = if(a == b) true else super.compare(a, b)
    }

    case class EqualToASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation("==", operands) with EqualToComparison

    case class GreaterThanOrEqualToASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation(">=", operands) {
        def performComparison[T](a: Ordered[T], b: T) = a < b
    }

    case class GreaterThanASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation(">", operands) {
        def performComparison[T](a: Ordered[T], b: T) = a < b
    }

    case class LogicalANDASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation("and", operands) with EqualToComparison {
        override def apply(context: JValue): Boolean = operands forall { value: ASTValue => compare(true, value(context)) }
    }

    case class LogicalORASTOperation(operands: List[ASTValue]) extends ComparativeASTOperation("or", operands) with EqualToComparison {
        override def apply(context: JValue): Boolean = operands exists { value: ASTValue => compare(true, value(context)) }
    }

    abstract sealed class SetComparativeOperation(operation: String, operands: List[ASTValue]) extends ASTOperation(operation, operands) {
        def performComparison[T](set: Seq[T], item: T): Boolean
        def performSliceComparison[T](set: Seq[T], slice: Seq[T]): Boolean

        def compare(set: Any, item: Any) = (set, item) match {
            case (set: String, item: String) => performSliceComparison(set, item)
            case (set: String, item: Int) => performSliceComparison(set, item.toString)
            case (set: String, item: Double) => performSliceComparison(set, item.toString)
            case (set: String, item: Boolean) => performSliceComparison(set, item.toString)
            case (set: Int, item: Int) => performSliceComparison(set.toString, item.toString)
            case (set: Int, item: String) => performSliceComparison(set.toString, item)
            case (set: Int, item: Double) => performSliceComparison(set.toString, item.toString)
            case (set: Int, item: Boolean) => performSliceComparison(set.toString, if(item) 1.toString else 0.toString)
            case (set: Double, item: Double) => performSliceComparison(set.toString, item.toString)
            case (set: Double, item: String) => performSliceComparison(set.toString, item)
            case (set: Double, item: Int) => performSliceComparison(set.toString, item.toString)
            case (set: Double, item: Boolean) => performSliceComparison(set.toString, if(item) 1.0d.toString else 0.0d.toString)
            case (set: Boolean, item: Boolean) => performSliceComparison(set.toString, item.toString)
            case (set: Boolean, item: String) => performSliceComparison(set.toString, item)
            case (set: Boolean, item: Int) => performSliceComparison(set.toString, if(item == 1) true.toString else false.toString)
            case (set: Boolean, item: Double) => performSliceComparison(set.toString, if(item == 1.0) true.toString else false.toString)
            case (set @ List(_*), item @ List(_*)) => performSliceComparison(set, item)
            case (set @ List(_*), item) => performComparison(set, item)
            case _ => false
        }
    }

    case class InOperation(operands: List[ASTValue]) extends SetComparativeOperation("in", operands) {
        def performComparison[T](set: Seq[T], item: T) = set contains item

        def performSliceComparison[T](set: Seq[T], slice: Seq[T]) = set containsSlice slice

        def apply(context: JValue) = {
            val item = operands.head(context)
            operands.tail forall { set: ASTValue => compare(set(context), item) }
        }
    }

    case class ContainsOperation(override val operands: List[ASTValue]) extends InOperation(operands.reverse)

    case class StartsWithOperation(override val operands: List[ASTValue]) extends ContainsOperation(operands) {
        override def performComparison[T](set: Seq[T], item: T) = set.head == item

        override def performSliceComparison[T](set: Seq[T], slice: Seq[T]) = set startsWith slice
    }

    case class EndsWithOperation(override val operands: List[ASTValue]) extends StartsWithOperation(operands) {
        override def performSliceComparison[T](set: Seq[T], slice: Seq[T]) = set endsWith slice
    }

    sealed abstract class TransformativeASTOperation(operation: String, operands: List[ASTValue]) extends ASTOperation(operation, operands) {
        def apply(context: JValue): Any
    }

    sealed abstract class ReductiveASTOperation(operation: String, operands: List[ASTValue]) extends ASTOperation(operation, operands) {
        def apply(context: JValue): List[Any]
    }
}

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

abstract class JSONFileDataSource(override val connectionString: String) extends JSONDataSource(connectionString) {

    override lazy val defaultParametersMap: Map[String, Any] = Map("maxWaitForLock" -> 100)    
    val maxWaitForLock = parametersMap.get("maxWaitForLock").get match { case i: Int => i }
    val fileName = parametersMap.get("fileName").get match { case s: String => s } //TODO: URL Decode

    val sourceLock = new ReentrantReadWriteLock()
    val readLock = sourceLock.readLock()
    val writeLock = sourceLock.writeLock()

    override protected def readData = {
        if(!readLock.tryLock(maxWaitForLock, TimeUnit.MILLISECONDS)) throw new Exception("Unable to obtain Read Lock")
        try {
            parse(fromFile(fileName).mkString) }
        catch  {
            case e: FileNotFoundException => JNothing
            case e: ParseException => JNothing
        } finally {
            readLock.unlock()
        }
    }

    protected def writeData(data: JValue) {
        if(!writeLock.tryLock(maxWaitForLock, TimeUnit.MILLISECONDS)) throw new Exception("Unable to obtain Read Lock")
        val writer = new FileWriter(new File(fileName))
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

    /**
     * TODO: Revise this documentation
     *
     * Filters following a fairly simple format. Each Filter consists of a tuple containing:
     *
     * - A JArray element used to locate the property on the to be checked on the item being filtered
     * - A JString element used to determine the comparison operation to be used
     * - A JValue element used to compare the property value against
     *
     *
     * The JArray element follows a fairly simply format in that the only valid values to be contained within
     * the JArray are JStrings and JInt values.
     *
     * A JString value is used to resolve a property name on the item being filtered and is applicable
     * when the base item is a JObject.
     *
     * A JInt value is used to resolve a value by numeric index and is applicable when the base item
     * is a JArray
     *
     *
     * The JString element may be one of: > >= = <= < in or like with the the ! (not) suffix being
     * valid for each of these.
     *
     *
     * The JValue element may be any valid JValue sub-class for which a comparison option is valid
     *
     * TODO: Detail the manner in which the comparison operators interact with various values
     */
    def getItemsById(itemIds: Seq[JSONQueryASTValue], contentFilters: Seq[JSONQueryASTValue], orderBy: Seq[JSONQueryASTValue], itemRange: (BigInt, BigInt)) = null

    def getItemsByFilter(itemFilters: Seq[JSONQueryASTValue], contentFilters: Seq[JSONQueryASTValue], orderBy: Seq[JSONQueryASTValue], itemRange: (BigInt, BigInt)) = null

    def updateItemsById(items: Seq[(_root_.net.liftweb.json.JValue, _root_.net.liftweb.json.JValue)]) {}

    def updateItemsByFilter(item: _root_.net.liftweb.json.JObject, contentFilters: Seq[JSONQueryASTValue]) {}

    def deleteItemsByFilter(contentFilters: Seq[JSONQueryASTValue]) {}
}

/**
 * A Stupid JSON Object DataSource
 *
 * Works with a file that contains a single JSON object. Each key on the object maps to a single JSON object.
 * The objects stored on the object do not need to be uniformly structured
 */
class JSONObjectFileDataSource(override val connectionString: String) extends JSONFileDataSource(connectionString) with JSONObjectDataSource {
    override lazy val connectionStringPattern = """file://(?<fileName>[^\?]+)"""
}

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
