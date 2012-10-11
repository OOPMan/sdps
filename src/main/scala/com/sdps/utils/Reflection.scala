package com.sdps.utils

//TODO: Add copyright
//TODO: Reference http://www.familie-kneissl.org/Members/martin/blog/reflection-from-scala-heaven-and-hell
//TODO: Document that this code does not handle default values well

object Reflection {
    implicit def string2Class[T <: AnyRef](name: String)(implicit classLoader: ClassLoader): Class[T] = {
        val clazz = Class.forName(name, true, classLoader)
        clazz.asInstanceOf[Class[T]]
    }

    def New[T <: AnyRef](className: String)(args: Any*)(implicit classLoader: ClassLoader): T = {
        val clazz: Class[T] = className
        val argsWithType: List[WithType] = args +: Nil
        val argTypes = argsWithType map {
            _.clazz
        } toArray
        val candidates = clazz.getConstructors filter {
            cons => matchingTypes(cons.getParameterTypes, argTypes)
        }
        require(candidates.length == 1, "Argument runtime types must select exactly one constructor")
        val params = argsWithType map {
            _.value
        }
        candidates.head.newInstance(params: _*).asInstanceOf[T]
    }

    private def matchingTypes(declared: Array[Class[_]], actual: Array[Class[_]]): Boolean = {
        declared.length == actual.length && (
                (declared zip actual) forall {
                    case (declared, actual) => declared.isAssignableFrom(actual)
                })
    }

    sealed abstract class WithType {
        val clazz: Class[_]
        val value: AnyRef
    }

    case class ValWithType(anyVal: AnyVal, clazz: Class[_]) extends WithType {
        lazy val value = toAnyRef(anyVal)
    }

    case class RefWithType(anyRef: AnyRef, clazz: Class[_]) extends WithType {
        val value = anyRef
    }
    
    implicit def ListAnyToListWithType(l: List[Any]): List[WithType] = l map {
        case i: AnyRef => RefWithType(i, i.getClass)
        case i: AnyVal => ValWithType(i, getType(i))
    }

    implicit def refWithType[T <: AnyRef](x: T) = RefWithType(x, x.getClass)

    implicit def valWithType[T <: AnyVal](x: T) = ValWithType(x, getType(x))

    def getType(x: AnyVal): Class[_] = x match {
        case _: Byte => java.lang.Byte.TYPE
        case _: Short => java.lang.Short.TYPE
        case _: Int => java.lang.Integer.TYPE
        case _: Long => java.lang.Long.TYPE
        case _: Float => java.lang.Float.TYPE
        case _: Double => java.lang.Double.TYPE
        case _: Char => java.lang.Character.TYPE
        case _: Boolean => java.lang.Boolean.TYPE
        case _: Unit => java.lang.Void.TYPE
    }

    def toAnyRef(x: AnyVal): AnyRef = x match {
        case x: Byte => Byte.box(x)
        case x: Short => Short.box(x)
        case x: Int => Int.box(x)
        case x: Long => Long.box(x)
        case x: Float => Float.box(x)
        case x: Double => Double.box(x)
        case x: Char => Char.box(x)
        case x: Boolean => Boolean.box(x)
        case x: Unit => ()
    }
}