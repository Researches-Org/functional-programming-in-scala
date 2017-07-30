package chapter09

trait Parsers[ParseError, Parser[+_]] {

  // This introduces the name self to refer to this Parsers instance; it's used later in ParserOps.
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  /**
    * The char function should satisfy an obvious law—for any Char, c:
    *
    * run(char(c))(c.toString) == Right(c)
    *
    * @param c create a Parser for char c.
    * @return Parser[Char]
    */
  def char(c: Char): Parser[Char]

  /**
    * We expect that or(string("abra"),string("cadabra")) will succeed whenever either string
    * parser succeeds:
    *
    * run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
    * run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
    *
    * @param s1 a Parser
    * @param s2 other Parser
    * @tparam A a type parameter for the parsers
    * @return a Parser that succeeds if s1 or s2 succeed
    */
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  /**
    * Here are some examples of what we expect from listOfN:
    *
    * run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    * run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    * run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
    *
    * @param n Number of repetitions
    * @param p a Parser
    * @tparam A type parameter of the Parser
    * @return a Parser that accepts repetitions
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  /**
    * should satisfy an obvious law—for any String, s:
    *
    * run(string(s))(s) == Right(s)
    *
    * @param s create a Parser for String s
    * @return Parser[String]
    */
  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // Use self to explicitly disambiguate reference to the or method on the trait.
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }

}
