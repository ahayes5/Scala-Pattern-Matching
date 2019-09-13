import java.util.Scanner

import scala.util.parsing.combinator._


abstract class Tree

case class Par(in: Tree) extends Tree

case class Opt(in: Tree) extends Tree

case class Concat(l: Tree, r: Tree) extends Tree

case class Or(l: Tree, r: Tree) extends Tree

case class Letter(n: Char) extends Tree


class Combinators extends JavaTokenParsers
{


	def s: Parser[Tree] = e

	def e: Parser[Tree] = t ~ "|" ~ e ^^
		{ case l ~ p ~ r => Or(l, r) } | t

	def t: Parser[Tree] = f ~ t ^^
		{ case l ~ r => Concat(l, r) } | f

	def f: Parser[Tree] = a ~ "?" ^^
		{ case l ~ p => Opt(l) } | a

	def a: Parser[Tree] = "(" ~ e ~ ")" ^^
		{ case l ~ p ~ r => Par(p) } | c

	def c: Parser[Tree] = "[A-Za-z0-9.]".r ^^
		{ str => Letter(str.charAt(0)) }

}

object Main extends Combinators
{

	var index = 0
	var matched = ""
	var matches = true

	def eval1(t: Tree, str: String): Boolean =
	{
		index = 0
		matched = ""
		matches = true
		eval(t, str)
		if (matched.equals(str))
			matches
		else
			false
	}

	def eval(t: Tree, str: String): Any =
		t match
		{

			case Par(in) => eval(in, str)

			case Opt(in) =>
			{
				val a = eval(in, str)
		//		if ()

			}

			case Concat(l, r) =>
			{
				eval(l, str)
				index += 1
				eval(r, str)
			}

			case Or(l, r) =>
			{
				val t = matches
				val a = eval(l, str)
				if (matches)
				{
					true
				}
				else
				{
					matches = t
					val b = eval(r, str)
					true
				}
			}

			case Letter(n) =>
			{
				try
				{
					if (str.charAt(index) == n)
					{
						matched += n.toString
						n
					}
					else
					{
						matches = false
						n
					}
				}
				catch
				{
					case estr: java.lang.StringIndexOutOfBoundsException => matches = false; false
				}
			}

		}

	def ev(t: Tree, s: String): Boolean =
	{
		matches = true
		index = 0
		matched = ""
		ev1(t, s)
		matches
	}

	def ev1(t: Tree, s: String): Any =
		t match
		{
			case Par(in) => ev1(in, s)
			case Opt(in) =>
			{
				ev1(in,s)
			}
			case Concat(l, r) =>
			{
				ev1(l,s)
				index+=1
				ev1(r,s)
			}
			case Or(l, r) =>
			{
				ev1(l,s)
			}
			case Letter(n) =>
			{
				if(s.substring(index,index+1).equals(n.toString))
					{
						matched+=n.toString
						n
					}

			}
		}

	override def skipWhitespace: Boolean = false

	def main(args: Array[String])
	{

		println("abc".substring(1,1))
		val exp: Tree = parseAll(s, "a|b").get

		println(exp)
		val sc = new Scanner(System.in)
		var ans = sc.nextLine()


		while (!ans.equals("exit"))
		{
			var a = ev(exp, ans)
			println(a)
			ans = sc.nextLine()

		}
	}
}