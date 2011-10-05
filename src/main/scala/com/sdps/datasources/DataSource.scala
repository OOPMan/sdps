package com.sdps.datasources

import net.liftweb.json._
import java.net.URI

/**
 * Created by IntelliJ IDEA.
 * User: adamj
 * Date: 9/12/11
 * Time: 9:29 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class DataSource(val uri: URI) {

    def getItemsById(ids: Seq[JValue] = Nil, contentFilters: Seq[JArray] = Nil): Seq[(JValue, JValue)]

    /**
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
    def getItemsByFilter(itemFilters: Seq[(JArray, JString, JValue)] = Nil, contentFilters: Seq[JArray] = Nil): Seq[(JValue, JValue)]

    def addItems(items: Seq[JValue]): Seq[JValue]

    def updateItems(items: Seq[(JValue, JValue)])

    def deleteItemsById(ids: Seq[JValue] = Nil)

}
