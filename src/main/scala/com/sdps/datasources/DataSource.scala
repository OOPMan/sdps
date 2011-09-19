package com.sdps.datasources

import net.liftweb.json.JsonAST.{JObject, JValue, JString}

/**
 * Created by IntelliJ IDEA.
 * User: adamj
 * Date: 9/12/11
 * Time: 9:29 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class DataSource(val uri: String) {

    def getItemsById(ids: Seq[JValue] = Nil, attributes: Seq[JString] = Nil): Seq[JObject]

    def getItemsByFilter(filters: Seq[(JString, JString, JValue)] = Nil, attributes: Seq[JString] = Nil): Seq[JObject]

    def addItems(items: Seq[JObject]): Seq[JValue]

    def updateItems(items: Seq[JObject])

    def deleteItemsById(ids: Seq[JValue] = Nil)

}