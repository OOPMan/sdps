package com.sdps.test.datasources

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
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

    val tempFile = File.createTempFile("JSONObjectFileDataSourceTest", ".json")
    val dataSource = new JSONObjectFileDataSource(tempFile.getAbsolutePath)
    var idObjectList: Seq[(JValue, JValue)] = Nil
    var idObjectMap: Map[JValue, JValue] = Map()
    implicit val formats = DefaultFormats
    Either

    val jsonObjects = List(
        ("field1" -> "string1") ~ ("field2" -> 1) ~ ("field3" -> 1.0) ~ ("field4" -> true)  ~ ("field5" -> List(1,2,3)) ~ ("field6" -> ("a" -> 1) ~ ("b" -> 2) ~  ("c" -> 3)),
        ("field1" -> "string2") ~ ("field2" -> 2) ~ ("field3" -> 2.0) ~ ("field4" -> false) ~ ("field5" -> List(2,3,4)) ~ ("field6" -> ("a" -> 2) ~ ("b" -> 4) ~  ("c" -> 6)),
        ("field1" -> "string3") ~ ("field2" -> 3) ~ ("field3" -> 3.0) ~ ("field4" -> true)  ~ ("field5" -> List(3,4,5)) ~ ("field6" -> ("a" -> 3) ~ ("b" -> 6) ~  ("c" -> 9)),
        ("field1" -> "string4") ~ ("field2" -> 4) ~ ("field3" -> 4.0) ~ ("field4" -> false) ~ ("field5" -> List(4,5,6)) ~ ("field6" -> ("a" -> 4) ~ ("b" -> 8) ~  ("c" -> 12)),
        ("field1" -> "string5") ~ ("field2" -> 5) ~ ("field3" -> 5.0) ~ ("field4" -> true)  ~ ("field5" -> List(5,6,7)) ~ ("field6" -> ("a" -> 5) ~ ("b" -> 10) ~ ("c" -> 15)),
        ("name" -> "something different") ~ ("description" -> "xyz") ~ ("field3" -> 6.0)
    )

    protected def generateFilters(filters: (Int, JValue, JString, JValue)*) = for((count, left, comparator, right) <- filters) yield
        ("%s %-3s %s".format(compact(render(left)), comparator.s, compact(render(right))), count, (left, comparator, right) :: Nil)

    val itemFilters = generateFilters(
        (2, "field1" :: Nil, "<", "string3"),
        (3, "field1" :: Nil, "<=", "string3"),
        (1, "field1" :: Nil, "=", "string3"),
        (3, "field1" :: Nil, ">=", "string3"),
        (2, "field1" :: Nil, ">", "string3"),
        (1, "field1" :: Nil, "in", "string3string"),
        (5, "string", "in", "field1" :: Nil),
        (5, "field1" :: Nil, "like", "string\\d"),
        (4, "field1" :: Nil, "!<", "string3"),
        (3, "field1" :: Nil, "!<=", "string3"),
        (5, "field1" :: Nil, "!=", "string3"),
        (3, "field1" :: Nil, "!>=", "string3"),
        (4, "field1" :: Nil, "!>", "string3"),
        (5, "field1" :: Nil, "!in", "string3string"),
        (1, "string", "!in", "field1" :: Nil),
        (1, "field1" :: Nil, "!like", "string\\d"),
        (2, "field2" :: Nil, "<", 3),
        (3, "field2" :: Nil, "<=", 3),
        (1, "field2" :: Nil, "=", 3),
        (3, "field2" :: Nil, ">=", 3),
        (2, "field2" :: Nil, ">", 3),
        (1, "field2" :: Nil, "in", 3),
        (4, "field2" :: Nil, "!<", 3),
        (3, "field2" :: Nil, "!<=", 3),
        (5, "field2" :: Nil, "!=", 3),
        (3, "field2" :: Nil, "!>=", 3),
        (4, "field2" :: Nil, "!>", 3),
        (5, "field2" :: Nil, "!in", 3),
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