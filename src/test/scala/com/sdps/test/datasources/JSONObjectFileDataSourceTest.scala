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

package com.sdps.test.datasources

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import java.io.File
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import com.sdps.datasources.JSONObjectFileDataSource

/**
 * Created by IntelliJ IDEA.
 * User: adamj
 * Date: 10/9/11
 * Time: 9:05 AM
 * To change this template use File | Settings | File Templates.
 */

//TODO: Split Test up by creating abstract test concrete sub-classes
class JSONObjectFileDataSourceTest extends FunSuite with BeforeAndAfterAll {

    implicit protected def convertAnyToJValue(input: Any): JValue = input match {
        case s: String => s
        case i: Int => i
        case f: Float => f
        case d: Double => d
        case b: Boolean => b
        case l: List[Any] => l map convertAnyToJValue  // <- Sometimes type-erasure can actually be useful!
    }


    val tempFile = File.createTempFile("JSONObjectFileDataSourceTest", ".json")
    val dataSource = new JSONObjectFileDataSource(tempFile.getAbsolutePath)
    var idObjectList: Seq[(JValue, JValue)] = Nil
    var idObjectMap: Map[JValue, JValue] = Map()
    implicit val formats = DefaultFormats

    val jsonObjects = List(
        ("field1" -> "string1") ~ ("field2" -> 1) ~ ("field3" -> 1.0) ~ ("field4" -> true)  ~ ("field5" -> List(1,2,3)) ~ ("field6" -> ("a" -> 1) ~ ("b" -> 2) ~  ("c" -> 3)),
        ("field1" -> "string2") ~ ("field2" -> 2) ~ ("field3" -> 2.0) ~ ("field4" -> false) ~ ("field5" -> List(2,3,4)) ~ ("field6" -> ("a" -> 2) ~ ("b" -> 4) ~  ("c" -> 6)),
        ("field1" -> "string3") ~ ("field2" -> 3) ~ ("field3" -> 3.0) ~ ("field4" -> true)  ~ ("field5" -> List(3,4,5)) ~ ("field6" -> ("a" -> 3) ~ ("b" -> 6) ~  ("c" -> 9)),
        ("field1" -> "string4") ~ ("field2" -> 4) ~ ("field3" -> 4.0) ~ ("field4" -> false) ~ ("field5" -> List(4,5,6)) ~ ("field6" -> ("a" -> 4) ~ ("b" -> 8) ~  ("c" -> 12)),
        ("field1" -> "string5") ~ ("field2" -> 5) ~ ("field3" -> 5.0) ~ ("field4" -> true)  ~ ("field5" -> List(5,6,7)) ~ ("field6" -> ("a" -> 5) ~ ("b" -> 10) ~ ("c" -> 15)),
        ("name" -> "something different") ~ ("description" -> "xyz") ~ ("field3" -> 6.0)
    )

    protected def generateFilterString(left: JValue, operator: JString, right: JValue): String = operator.s.toLowerCase.stripPrefix(":").stripPrefix("!").stripSuffix(":") match {
        case "and" => (left, right) match { case (JArray(List(l1, JString(o1), r1)), JArray(List(l2, JString(o2), r2))) => "%s %-3s %s".format(generateFilterString(l1, o1, r1), operator.s, generateFilterString(l2, o2, r2)) }
        case "or" =>  (left, right) match { case (JArray(List(l1, JString(o1), r1)), JArray(List(l2, JString(o2), r2))) => "%s %-3s %s".format(generateFilterString(l1, o1, r1), operator.s, generateFilterString(l2, o2, r2)) }
        case _ => "%s %-3s %s".format(compact(render(left)), operator.s, compact(render(right)))
    }

    protected def generateFilters(filters: (Int, JValue, JString, JValue)*) = for((count, left, operator, right) <- filters) yield
        (generateFilterString(left, operator, right), count, (left, operator, right) :: Nil)

    val itemFilters = generateFilters(
        (6, "string", "=", "string"),
        (6, 1, "=", 1),
        (6, 1.5, "=", 1.5),
        (6, true, "=", true),
        (6, 1 :: 2 :: 3 :: Nil, ":=:", 1 :: 2 :: 3 :: Nil),
        (1, 1 :: "=" :: 1 :: Nil, "and", ("field1" :: Nil) :: "=" :: "string3" :: Nil),
        (0, JArray(List(JInt(1), JString("="), JInt(0))), "and", JArray(List(JArray("field1" :: Nil), JString("="), JString("string3")))),
        (1, JArray(List(JInt(1), JString("="), JInt(0))), "or", JArray(List(JArray("field1" :: Nil), JString("="), JString("string3")))),
        (2, "field1" :: Nil, "<", "string3"),
        (3, "field1" :: Nil, "<=", "string3"),
        (1, "field1" :: Nil, "=", "string3"),
        (3, "field1" :: Nil, ">=", "string3"),
        (2, "field1" :: Nil, ">", "string3"),
        (1, "field1" :: Nil, "in", "string3string"),
        (5, "string", "in", "field1" :: Nil),
        (5, "field1" :: Nil, "like", "string\\d"),
        (3, "field1" :: Nil, "!<", "string3"),
        (2, "field1" :: Nil, "!<=", "string3"),
        (4, "field1" :: Nil, "!=", "string3"),
        (2, "field1" :: Nil, "!>=", "string3"),
        (3, "field1" :: Nil, "!>", "string3"),
        (4, "field1" :: Nil, "!in", "string3string"),
        (0, "string", "!in", "field1" :: Nil),
        (0, "field1" :: Nil, "!like", "string\\d"),
        (2, "field2" :: Nil, "<", 3),
        (3, "field2" :: Nil, "<=", 3),
        (1, "field2" :: Nil, "=", 3),
        (3, "field2" :: Nil, ">=", 3),
        (2, "field2" :: Nil, ">", 3),
        (1, "field2" :: Nil, "in", 3),
        (3, "field2" :: Nil, "!<", 3),
        (2, "field2" :: Nil, "!<=", 3),
        (4, "field2" :: Nil, "!=", 3),
        (2, "field2" :: Nil, "!>=", 3),
        (3, "field2" :: Nil, "!>", 3),
        (4, "field2" :: Nil, "!in", 3),
        (2, "field3" :: Nil, "<", 3.0),
        (3, "field3" :: Nil, "<=", 3.0),
        (1, "field3" :: Nil, "=", 3.0),
        (4, "field3" :: Nil, ">=", 3.0),
        (3, "field3" :: Nil, ">", 3.0),
        (1, "field3" :: Nil, "in", 3.0),
        (4, "field3" :: Nil, "!<", 3.0),
        (3, "field3" :: Nil, "!<=", 3.0),
        (5, "field3" :: Nil, "!=", 3.0),
        (2, "field3" :: Nil, "!>=", 3.0),
        (3, "field3" :: Nil, "!>", 3.0),
        (5, "field3" :: Nil, "!in", 3.0),
        (2, "field4" :: Nil, "<", true),
        (5, "field4" :: Nil, "<=", true),
        (3, "field4" :: Nil, "=", true),
        (3, "field4" :: Nil, ">=", true),
        (0, "field4" :: Nil, ">", true),
        (3, "field4" :: Nil, "in", true)
    )
    //TODO: Object ItemFilter
    //TODO: Array ItemFilter

    //TODO: String ContentFilter
    //TODO: Int ContentFilter
    //TODO: Float ContentFilter
    //TODO: Boolean ContentFilter
    //TODO: Object ContentFilter
    //TODO: Array ContentFilter

    //TODO: OrderBy
    //TODO: itemRange

    test("read empty") {
        assert(dataSource.getItemsById().isEmpty, "dataSource was not empty")
    }

    test("write data") {
        idObjectList = dataSource.addItems(jsonObjects).zip(jsonObjects)
        idObjectMap = idObjectList.toMap
    }

    test("confirm written data") {
        val items = dataSource.getItemsById()
        expect(jsonObjects.length) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "read data does not match written data")
    }

    for((name, length, filter) <- itemFilters) {
        test("query for data with %s".format(name)) {
            val items = dataSource.getItemsByFilter(filter)
            expect(length) { items.length }
            if(length > 0) assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with %s failed".format(name))
        }
    }

    test("filter written data") {

    }

    test("order written data") {

    }

    test("range written data") {

    }

    test("query and filter written data") {

    }

    test("query and order written data") {

    }

    test("query and range written data") {

    }

    test("filter and order written data") {

    }

    test("filter and range written data") {

    }

    test("order and range written data") {

    }

    test("update written data") {

    }

    test("confirm update to written data") {

    }

    test("delete from written data") {

    }

    test("confirm delete from written data") {

    }

    override def afterAll(configMap: Map[String, Any]) {
        tempFile.delete()
    }

}