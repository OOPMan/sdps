package com.sdps.datasources

import scala.io.Source.fromURI
import net.liftweb.json._
import java.net.URI
import java.io.{File, FileWriter}
import java.util.UUID

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
class StupidJSONDataSource(override val uri: URI) extends DataSource(uri) {

    protected def readData: JObject = parse(fromURI(uri).mkString) match { case o: JObject => o }

    protected def writeData(data: JObject) {
        val writer = new FileWriter(new File(uri))
        try { writer.write(compact(render(data))) }
        finally { writer.close() }
    }

    def getItemsById(ids: Seq[JValue] = Nil, attributes: Seq[JString] = Nil) = for {
            JField(name, value: JObject) <- readData.obj
            if ids contains name
            final_value = if(attributes.length == 0) value else new JObject(value.obj filter { case JField(fieldname, _) => attributes contains fieldname } )
        } yield final_value

    //TODO: Implement
    def getItemsByFilter(filters: Seq[(JString, JString, JValue)], attributes: Seq[JString]) = null

    def addItems(items: Seq[JObject]) = {
        val newItems = new JObject(Nil ++ items map { new JField(UUID.randomUUID.toString, _) })
        writeData(readData merge newItems match { case o: JObject => o })
        newItems.obj.map { (f: JField) => new JString(f.name) }
    }

    //TODO: Implement
    def updateItems(items: Seq[(JValue, JObject)]) = null

    //TODO: Implement
    def deleteItemsById(ids: Seq[JValue]) = null
}