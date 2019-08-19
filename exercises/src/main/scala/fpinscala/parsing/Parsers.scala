package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  // def many[A](p: Parser[A]): Parser[List[A]]
  def many1[A](p: Parser[A]): Parser[List[A]]
  // def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  // def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def map2viaProduct[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    product(p,p2) map { case (a,b) => f(a,b) }

  // with map2, or, and succeed
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty[A])

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List.empty)
    else map2(p, listOfN(n-1, p))(_ :: _)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p.flatMap(a => p2.map(b => (a,b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a,b)))

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(succeed[B] _ compose f)
  // p.flatMap(a => succeed(f(a)))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]) = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
