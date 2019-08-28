package fpinscala.parsing

trait JSON

trait JSONParsers[Parser[+JSON]] extends Parsers[Parser] {
  import JSON._
  def jsonString: Parser[JString]
  def jsonNumber: Parser[JNumber]
  def jsonBool: Parser[JBool]
  def jsonNull: Parser[JSON]
  def jsonWhitespace: Parser[String]
  def token: Parser[JSON] = {
    or(jsonNumber, or(jsonBool, or(jsonNull, or(jsonObject, or(jsonArray, jsonString)))))
  }
  def quote: Parser[String] = string("\"")
  def startArray: Parser[String] = string("[")
  def endArray: Parser[String] = string("]")
  def startObject: Parser[String] = string("{")
  def endObject: Parser[String] = string("}")
  def colon: Parser[String] = string(":")
  def comma: Parser[String] = string(",")
  def keepMiddle[A](start: Parser[_], middle: Parser[A], end: Parser[_]): Parser[A] =
    map2(start, map2(middle, end)((a,_) => a))((_,a) => a)
  def commaElems[A](parser: Parser[A]): Parser[List[A]] =
    or(
      map2(
        many(keepMiddle(jsonWhitespace, parser, comma)),
        parser
      )(_ :+ _),
      map(jsonWhitespace)(_ => List.empty)
    )
  def jsonArray: Parser[JArray] =
    map(keepMiddle(startArray, commaElems(token), endArray))(elems => JArray(elems.toIndexedSeq))
  def jsonKeyValue: Parser[(String, JSON)] =
    flatMap(jsonString)(s => flatMap(jsonWhitespace)( _ => flatMap(string(":"))(_ => map(token)(o => (s.get,o)))))
  def jsonObject: Parser[JObject] =
    map(keepMiddle(startObject, commaElems(jsonKeyValue), endObject))(kvPairs => JObject(kvPairs.toMap))
}

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: JSONParsers[Parser]): Parser[JSON] = {
    P.token
  }
}

