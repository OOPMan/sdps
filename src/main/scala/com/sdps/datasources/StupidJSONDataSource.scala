package com.sdps.datasources

import net.liftweb.json.JsonAST.{JString, JObject, JValue}

/**
 * Created by IntelliJ IDEA.
 * User: adamj
 * Date: 9/13/11
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 *
 * A Stupid JSON DataSource. Loads JSON data from a file. Very stupid :-)
 */
class StupidJSONDataSource(val uri: String) extends DataSource(uri) {

    def getItemsById(ids: Seq[JValue], attributes: Seq[JString]) = null

    def getItemsByFilter(filters: Seq[(JString, JString, JValue)], attributes: Seq[JString]) = null

    def addItems(items: Seq[JObject]) = null

    def updateItems(items: Seq[JObject]) = null

    def deleteItemsById(ids: Seq[JValue]) = null
}