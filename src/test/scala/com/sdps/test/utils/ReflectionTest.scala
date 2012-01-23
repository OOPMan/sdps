package com.sdps.test.utils

import java.io.File
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import com.sdps.datasources.JSONObjectFileDataSource
import com.sdps.utils.Reflection.{WithType, New}
import util.parsing.json.JSONObject
import tools.nsc.util.ScalaClassLoader.URLClassLoader
import java.net.{URL}

class ReflectionTest extends FunSuite with BeforeAndAfterAll {
    val tempFile = File.createTempFile("JSONObjectFileDataSourceTest", ".json")

    test("instantiate JSONObjectFileDataSource instance using New") {
        //TODO: We need a class loader here
        implicit val classLoader = getClass.getClassLoader
        val dataSource: JSONObjectFileDataSource = New("com.sdps.datasources.JSONObjectFileDataSource")(tempFile.getAbsolutePath, 100)
    }

    override def afterAll(configMap: Map[String, Any]) {
        tempFile.delete()
    }

}
