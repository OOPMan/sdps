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

    val jsonObjects = List(
        ("field1" -> "string1") ~ ("field2" -> 1) ~ ("field3" -> 1.0) ~ ("field4" -> true)  ~ ("field5" -> List(1,2,3)) ~ ("field6" -> ("a" -> 1) ~ ("b" -> 2) ~  ("c" -> 3)),
        ("field1" -> "string2") ~ ("field2" -> 2) ~ ("field3" -> 2.0) ~ ("field4" -> false) ~ ("field5" -> List(2,3,4)) ~ ("field6" -> ("a" -> 2) ~ ("b" -> 4) ~  ("c" -> 6)),
        ("field1" -> "string3") ~ ("field2" -> 3) ~ ("field3" -> 3.0) ~ ("field4" -> true)  ~ ("field5" -> List(3,4,5)) ~ ("field6" -> ("a" -> 3) ~ ("b" -> 6) ~  ("c" -> 9)),
        ("field1" -> "string4") ~ ("field2" -> 4) ~ ("field3" -> 4.0) ~ ("field4" -> false) ~ ("field5" -> List(4,5,6)) ~ ("field6" -> ("a" -> 4) ~ ("b" -> 8) ~  ("c" -> 12)),
        ("field1" -> "string5") ~ ("field2" -> 5) ~ ("field3" -> 5.0) ~ ("field4" -> true)  ~ ("field5" -> List(5,6,7)) ~ ("field6" -> ("a" -> 5) ~ ("b" -> 10) ~ ("c" -> 15)),
        ("name" -> "something different") ~ ("description" -> "xyz") ~ ("field3" -> 6.0)
    )

    //TODO: String ItemFilter
    // Int ItemFilter
    val filterForObjectsWithField2LessThan3 = (JArray("field2" :: Nil), JString("<"), JInt(3)) :: Nil
    val filterForObjectsWithField2LessThanOrEqualTo3 = (JArray("field2" :: Nil), JString("<="), JInt(3)) :: Nil
    val filterForObjectsWithField2EqualTo3 = (JArray("field2" :: Nil), JString("=="), JInt(3)) :: Nil
    val filterForObjectsWithField2GreaterThanEqualTo3 = (JArray("field2" :: Nil), JString(">="), JInt(3)) :: Nil
    val filterForObjectsWithField2GreaterThan3 = (JArray("field2" :: Nil), JString(">"), JInt(3)) :: Nil
    val filterForObjectsWithField2NotEqualTo3 = (JArray("field2" :: Nil), JString("!="), JInt(3)) :: Nil
    // Float ItemFilter
    val filterForObjectsWithField2LessThan3point0 = (JArray("field3" :: Nil), JString("<"), JDouble(3.0)) :: Nil
    val filterForObjectsWithField2LessThanOrEqualTo3point0 = (JArray("field3" :: Nil), JString("<="), JDouble(3.0)) :: Nil
    val filterForObjectsWithField2EqualTo3point0 = (JArray("field3" :: Nil), JString("=="), JDouble(3.0)) :: Nil
    val filterForObjectsWithField2GreaterThanEqualTo3point0 = (JArray("field3" :: Nil), JString(">="), JDouble(3.0)) :: Nil
    val filterForObjectsWithField2GreaterThan3point0 = (JArray("field3" :: Nil), JString(">"), JDouble(3.0)) :: Nil
    val filterForObjectsWithField2NotEqualTo3point0 = (JArray("field3" :: Nil), JString("!="), JDouble(3.0)) :: Nil
    //TODO: Boolean ItemFilter
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

    test("query for data with field2 < 3") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2LessThan3)
        expect(2) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 < 3 failed")
    }
    test("query for data with field2 <= 3") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2LessThanOrEqualTo3)
        expect(3) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 <= 3 failed")
    }
    test("query for data with field2 == 3") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2EqualTo3)
        expect(1) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 == 3 failed")
    }
    test("query for data with field2 >= 3") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2GreaterThanEqualTo3)
        expect(3) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 >= 3 failed")
    }
    test("query for data with field2 > 3") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2GreaterThan3)
        expect(2) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 > 3 failed")
    }

    test("query for data with field2 < 3.0") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2LessThan3point0)
        expect(2) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 < 3.0 failed")
    }
    test("query for data with field2 <= 3.0") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2LessThanOrEqualTo3point0)
        expect(3) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 <= 3.0 failed")
    }
    test("query for data with field2 == 3.0") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2EqualTo3point0)
        expect(1) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 == 3.0 failed")
    }
    test("query for data with field2 >= 3.0") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2GreaterThanEqualTo3point0)
        expect(4) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 >= 3.0 failed") //TODO: Fix
    }
    test("query for data with field2 > 3.0") {
        val items = dataSource.getItemsByFilter(filterForObjectsWithField2GreaterThan3point0)
        expect(3) { items.length }
        assert((for { (id, objekt) <- items } yield idObjectMap(id) == objekt) reduce { _ & _ }, "query for objects with field2 > 3.0 failed") //TODO: Fix
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