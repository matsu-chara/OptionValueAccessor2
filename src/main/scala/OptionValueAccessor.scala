import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox

object OptionValueAccessor {

  implicit def unwrapOptValue[A](a: OptValue[A]): Option[A] = a.unwrap

  implicit class OptValue[A](val unwrap: Option[A]) extends Dynamic  {
    def wrap: OptValue[A] = new OptValue[A](unwrap)
    def selectDynamic(name: String): Any = macro Impl.Opt.selectDynamicImpl
  }

  object Impl {
    object Opt {
      def selectDynamicImpl(c: whitebox.Context)(name: c.Expr[String]): c.Tree = {
        import c.universe._

        val nameStr = name.tree match {
          case pq"${n: String}" if n.startsWith("_") => n.drop(1)
          case _ => c.abort(c.enclosingPosition, s"#$name not found.")
        }

        c.typecheck(q"${c.prefix}.unwrap").tpe.typeArgs match {
          case h :: Nil if h <:< c.typecheck(q"scala.Option[Any](null)").tpe =>
            q"new OptionValueAccessor.OptValue(${c.prefix}.unwrap.flatten.map {_.${TermName(nameStr)}})"
          case _ =>
            q"new OptionValueAccessor.OptValue(${c.prefix}.unwrap.map {_.${TermName(nameStr)}})"
        }
      }
    }
  }
}
