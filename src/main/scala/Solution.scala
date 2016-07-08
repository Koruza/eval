import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
	def eval(e:Expr): Double = e match{
		case Add(h, t) => eval(h)+eval(t)
	    case Sub(h, t) => eval(h)-eval(t)
	    case Mul(h, t) => eval(h)*eval(t)
	    case Div(h, t) => eval(h)/eval(t)
	    case Exponent(h, t) => math.pow(eval(h),eval(t))
	    case Num(n) => n
	}
}

object ArithParser extends ArithParserLike {
	// number : PackratParser [ Double ] is defined in ArithParserLike
	// (parser ^^ transformation) == parser.map(transformation)
	// (parser ^^^ replacement) == parser.map(_ => replacement)
	// (parser >> nextStep) == parser.flatMap(nextStep)
	lazy val atom : PackratParser [ Expr ] = { (number) ^^ {case x => Num(x)} | ("("~expr~")") ^^ {case _~e~_ => e}}
	lazy val exponent : PackratParser [ Expr ] = {(exponent ~"^"~atom)^^{case h~"^"~t =>Exponent(h,t)} | atom}
	lazy val add : PackratParser [ Expr ] = { (mul ~"+"~add)^^{case h~"+"~t =>Add(h,t)} |
											  (mul ~"-"~add)^^{case h~"-"~t =>Sub(h,t)} | mul}
	lazy val mul : PackratParser [ Expr ] = { (exponent ~"*"~mul)^^{case h~"*"~t =>Mul(h,t)} |
											  (exponent ~"/"~mul)^^{case h~"/"~t =>Div(h,t)} | exponent}
	lazy val expr : PackratParser [ Expr ] = add
}

object ArithPrinter extends ArithPrinterLike {
	//returns strings that represent arithmetic expressions
	//parseExpr(print(e)) == e
	def print (e: Expr ): String = 
	e match{
		case Add(h, t) =>"("+ "("+print (h)+")" + "+" + "("+print(t)+")"+")"
	    case Sub(h, t) =>"("+ "("+print (h)+")" + "-" + "("+print(t)+")"+")"
	    case Mul(h, t) =>"("+ "("+print (h)+")" + "*" + "("+print(t)+")"+")"
	    case Div(h, t) =>"("+ "("+print (h)+")" + "/" + "("+print(t)+")"+")"
	    case Exponent(h, t) =>"("+ "("+print (h)+")" + "^" + "("+print(t)+")"+")"
	    case Num(n) => n.toString
	}

}