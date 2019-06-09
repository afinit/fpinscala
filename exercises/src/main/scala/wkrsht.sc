import scala.util.Random

import scala.annotation.tailrec

def fib(n: Int): Int = {
  @tailrec
  def go(pnum1: Int, pnum2: Int, idx: Int): Int = {
    if (idx >= n) pnum1
    else go(pnum2, pnum1 + pnum2, idx + 1)
  }
  if (Vector(0,1).contains(n)) n
  else go(0,1,0)
}

fib(5)
fib(6)
fib(8)
fib(9)



/*
import scala.util.Try
import scala.util.Success
import scala.util.Failure

case class Person(name: String, age: Int)

val ppl = Vector(
  Person("h", 5),
  Person("h", 99),
  Person("j", 2),
  Person("k", 9),
  Person("l", 7)
)

def dumbFun(prsn: Person): Person = prsn match {
  case Person("j", age) => throw new IllegalArgumentException(s"j not allowed. age: $age")
  case _ => prsn
}

val donePpl = ppl.map( p => Try(dumbFun(p)))


//donePpl.foreach(_.foreach(p => println(p.name)))

//donePpl.collect { case Success(p) => p } groupBy(_.name)
//val (s,f) = donePpl.partition {
//  case Success(_) => true
//  case _ => false
//}

donePpl.map {
  case Failure(ex) => println(s"Failure: $ex")
  case Success(p) => p
} filterNot(_ == ())

*/
//import scala.util.control.Exception.catching
//catching(Throwable)
//donePpl.groupBy(_.flatMap(_.name))

//Try(dumbFun(ppl.head)).flatMap(_.name)

//for {
//  pAttempt: Try[Person] <- donePpl
//  p: Person <- pAttempt
//} yield p.name


def gcd(x: Int, y: Int): Int = {
  if (x < y) gcd(y,x)
  else if (y == 0) x
  else gcd(y, x % y)
}

gcd(5,3)
gcd(18,98)
gcd(9234942,112314)
gcd(0,59)

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import scala.util.Try

object Gzip {
  // code from https://gist.github.com/owainlewis/1e7d1e68a6818ee4d50e

  def compress(input: Array[Byte]): Array[Byte] = {
    val bos = new ByteArrayOutputStream(input.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(input)
    gzip.close()
    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  def decompress(compressed: Array[Byte]): Option[String] =
    Try {
      val inputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
      scala.io.Source.fromInputStream(inputStream).mkString
    }.toOption
}

def bwt(r: String): String = {
  val s = r + "$"
  (0 until s.length)
    .sortWith((a, b) => s.charAt(a) < s.charAt(b))
    .map(i => s.charAt( Math.floorMod(i - 1, s.length))).mkString("")
}

val banana = bwt("banana")
val longerStr = bwt("today i am but a man, but tomorrow i will climb a tree.. ")
//val dna = "ATTATATCTAGATCGGCATCGAACACACCTACTACGGAGA"

/*
val dna = (0 until 10000).toVector
  .map(_ => Random.nextInt(4))
  .map(i => "ACDEFGHIKLMNPQRSTVWY"(i))
  .mkString("")


val dnaBwt = bwt(dna)
val compress = Gzip.compress((dna + "$").getBytes("UTF-8")).size
val compressBwt = Gzip.compress(dnaBwt.getBytes("UTF-8")).size
*/


