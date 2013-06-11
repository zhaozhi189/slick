package scala.slick.yy

import scala.reflect.macros.Context
import scala.reflect.runtime.universe.definitions.FunctionClass
import ch.epfl.yinyang.typetransformers.{ PolyTransformer }

class SlickTypeTransformer[C <: Context](ctx: C) extends PolyTransformer[C](ctx) {
  import c.universe._

  override def transform(typeCtx: TypeContext, inType: Type): Tree = {
    println(("!" * 5) + s"""handling: $inType in $typeCtx with name ${inType.typeSymbol.name}""")
    val res = typeCtx match {
      case TypeApplyCtx => TypeTree(inType)
      case OtherCtx => inType match {
        case TypeRef(pre, sym, args) if !args.isEmpty && !isFunctionType(inType) => {
          val liftedArgs =
            args map { x => TypeTree(x) }
          AppliedTypeTree(Select(This(newTypeName(className)), toType(sym)),
            liftedArgs)
        }
        case _ => super.transform(typeCtx, inType)
      }
    }
    println(("!" * 5) + s"""res for: $inType in $typeCtx is ${showRaw(res)}""")
    res
  }

}