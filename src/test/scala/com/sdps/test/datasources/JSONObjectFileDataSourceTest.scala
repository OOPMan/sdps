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

class JSONObjectFileDataSourceTest extends FunSuite with BeforeAndAfterAll {

    val tempFile = File.createTempFile("JSONObjectFileDataSourceTest", ".json")
    val dataSource = new JSONObjectFileDataSource(tempFile.getAbsolutePath)
    var idObjectList: Seq[(JValue, JValue)] = Nil
    var idObjectMap: Map[JValue, JValue] = Map()
    implicit val formats = DefaultFormats

    case class ObjectA(field1: String, field2: Int, field3: Float, field4: Boolean, field5: List[Int])
    case class ObjectB(name: String, description: Option[String], field3: Float)

    val jsonObjects = List(
        ("field1" -> "string1") ~ ("field2" -> 1) ~ ("field3" -> 1.0) ~ ("field4" -> true) ~ ("field5" -> List(1,2,3)),
        ("field1" -> "string2") ~ ("field2" -> 2) ~ ("field3" -> 2.0) ~ ("field4" -> false) ~ ("field5" -> List(2,3,4)),
        ("field1" -> "string3") ~ ("field2" -> 3) ~ ("field3" -> 4.0) ~ ("field4" -> true) ~ ("field5" -> List(3,4,5)),
        ("field1" -> "string4") ~ ("field2" -> 4) ~ ("field3" -> 5.0) ~ ("field4" -> false) ~ ("field5" -> List(4,5,6)),
        ("field1" -> "string5") ~ ("field2" -> 5) ~ ("field3" -> 6.0) ~ ("field4" -> true) ~ ("field5" -> List(5,6,7)),
        ("name" -> "something different") ~ ("description" -> (None: Option[String])) ~ ("field3" -> 6.0)
    )

    val filterForObjectsWithField2EqualTo3 = (JArray("field2" :: Nil), JString("=="), JInt(3)) :: Nil

    test("read empty") {
        assert(dataSource.getItemsById().isEmpty, "dataSource was not empty")
    }

    test("write data") {
        idObjectList = dataSource.addItems(jsonObjects).zip(jsonObjects)
        idObjectMap = idObjectList.toMap
    }

    test("confirm written data") {
        assert(
            (for {
                ((writtenObjectId, writtenObject), (readObjectId, readObject)) <- idObjectList zip dataSource.getItemsById()
            } yield (readObject == writtenObject) && (writtenObjectId == readObjectId)) contains false,
            "read data does not match written fixture")
    }

    test("query written data") {
        // TODO: Test ALL operators
        assert(
            (for { (id, objekt) <- dataSource.getItemsByFilter(filterForObjectsWithField2EqualTo3) } yield idObjectMap(id) == objekt) reduce { _ & _ },
            "query for objects with field2 equal to 3 failed")
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