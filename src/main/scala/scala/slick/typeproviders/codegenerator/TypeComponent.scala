package scala.slick.typeproviders.codegenerator

import scala.reflect.runtime.universe._

trait TypeComponent { self: Generator =>
  def generateCodeForTypeTree(tpt: Tree): String =
    tpt.toString

  def generateCodeForTypeDef(typeDef: TypeDef): String = {
    val TypeDef(_, TypeName(typeName), List(), typeType) = typeDef
    s"${genIndent}type $typeName = ${generateCodeForTypeTree(typeType)}"
  }
}