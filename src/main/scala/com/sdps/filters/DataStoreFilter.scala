package com.sdps.filters

//TODO: Add copyright

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport
import scala.io.Source.fromFile
import net.liftweb.json._
import com.sdps.utils.Reflection.New
import com.sdps.datasources.DataSource
import tools.nsc.util.ScalaClassLoader.URLClassLoader

class DataStoreFilter extends SDPSFilter with ScalateSupport {
    implicit val formats = DefaultFormats
    //TODO: Complete case class
    case class DataSourceConfig(clazz: String, connectionString: String, additionalArguments: Option[List[Any]], classpath: Option[String])
    // Load JSON Config file
    //TODO: We need to load datastorefilter.json from the WEB-INF folder
    val configData = parse(fromFile("datastorefilter.json").mkString) match { case o: JObject => o }
    // Parse JSON Config data
    val dataSourceConfigs = for(f <- configData.obj) yield f.name -> f.value.extract[DataSourceConfig]
    //Construct custom classloader using all additional classpaths
    val dataSourceConfigsWithClasspaths = dataSourceConfigs filter { case (_, dataSourceConfig: DataSourceConfig) => dataSourceConfig.classpath.isDefined } 
    val classpathURLs = dataSourceConfigsWithClasspaths map { case (_, dataSourceConfig: DataSourceConfig) => new URL(dataSourceConfig.classpath.get) }
    implicit val classLoader = new URLClassLoader(classpathURLs, getClass.getClassLoader)
    // Parse Data Sources
    for((name, dataSourceConfig) <- dataSourceConfigs)
    {
        val constructorParameters = dataSourceConfig.connectionString +: dataSourceConfig.additionalArguments.getOrElse { Nil }
        val ds: DataSource = New(dataSourceConfig.clazz)(constructorParameters: _*)
        //TODO: Generate relevant get/post method calls
    }

    get("/") {
        <html>
            <body>
                <h1>Hello, world!</h1>
                Say
                <a href="hello-scalate">hello to Scalate</a>
                .
            </body>
        </html>
    }

}
