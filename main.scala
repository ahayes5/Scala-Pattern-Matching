import java.util.Scanner

import scala.util.parsing.combinator._


abstract class Tree

case class Par(in: Tree) extends Tree

case class Opt(in: Tree) extends Tree

case class Concat(l: Tree, r: Tree) extends Tree

case class Or(l: Tree, r: Tree) extends Tree

case class Letter(n: String) extends Tree


class Combinators extends JavaTokenParsers
{


	def s: Parser[Tree] = e

	def e: Parser[Tree] = t ~ "|" ~ e ^^
		{ case l ~ p ~ r => Or(l, r) } | t

	def t: Parser[Tree] = f ~ t ^^
		{ case l ~ r => Concat(l, r) } | f

	def f: Parser[Tree] = a ~ "?" ^^
		{ case l ~ p => Or(l, Letter("")) } | a

	def a: Parser[Tree] = "(" ~ e ~ ")" ^^
		{ case l ~ p ~ r => Par(p) } | c

	def c: Parser[Tree] = "[A-Za-z0-9. ]".r ^^
		{ str => Letter(str.substring(0, 1)) }


}

object Main extends Combinators
{

	var index = 0
	//var position=0
	//var matched = ""
	//var matches = true

	def eval1(t: Tree, str: String): Boolean =
	{
		index = 0
		//position=0
		//matched = ""
		var matches = true

		val a = eval(t, str)
		//println(a)
		if (a.equals(str))
			matches
		else
			false
	}

	def eval(t: Tree, str: String): String =
		t match
		{

			case Par(in) => eval(in, str)


			case Concat(l, r) =>
			{

				val a = eval(l, str)
				if (a.equals(""))
					index -= 1
				index += 1
				a + eval(r, str)
			}

			case Or(l, r) =>
			{
				val position = index
				val a = eval(l, str)
				val tempindex = index
				index = position
				val b = eval(r, str)

				if ((tempindex < str.length) && a.equals(str.substring(position, tempindex + 1)))
				{
					index = tempindex
					a
				}
				else if ((tempindex < str.length) && a.equals(str.substring(position)))
				{
					index = tempindex
					a
				}
				else if (position>= str.length)
					"NULL"
				else if ((tempindex >= str.length) && a.equals(str.substring(position)))
					a
				else if ((index < str.length) && b.equals(str.substring(position, index + 1)))
					b
				else if ((index < str.length) && b.equals(str.substring(position)))
					b
				else if ((index >= str.length) && b.equals(str.substring(position)))
					b
				else if (b.equals("EMPTY"))
					""
				else
					"NULL"

			}

			case Letter(n) =>
			{
				if (n.equals(""))
				{
					"EMPTY"
				}
				else
				{
					if (index < str.length)
					{
						val a = str.substring(index, index + 1)


						if (n.equals(a))
						{
							//matched += n
							n
						}
						else if (n.equals("."))
							str.substring(index, index + 1)
						else
						{
							//matches = false
							"NULL"
						}
					}
					else
						"NULL"
				}
			}

		}


	override def skipWhitespace: Boolean = false

	def main(args: Array[String])
	{
		val sc = new Scanner(System.in)
		print("Pattern: ")
		val pat = sc.nextLine()

		val exp: Tree = parseAll(s, pat).get

		println(exp)
		print("String: ")
		var ans = sc.nextLine()


		while (!ans.equals("exit"))
		{
			var a = eval1(exp, ans)
			if (a)
				println("match")
			else
				println("no match")
			print("String: ")
			ans = sc.nextLine()

		}
	}
}
