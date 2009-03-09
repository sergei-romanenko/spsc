package spsc;

import scala.util.parsing.combinator.Parsers

trait StrongParsers extends Parsers{
  
  // A parser generator that corresponds to p+~EOF
  // but instead of returning "EOF expected" at the middle of the file it reports where p has failed.
  def strongRep1[T](p: => Parser[T]): Parser[List[T]] = new Parser[List[T]]{
    def apply(in0: Input)  = {
      val xs = new scala.collection.mutable.ListBuffer[T]
      var in = in0
      var res: ParseResult[T] = null
      do {
        res = p(in)
        in = res.next
        if (res.successful) xs+=res.get
      } while (res.successful && !res.next.atEnd)
      res match {
        case s @ Success(out, in1) => Success(xs.toList, res.next)
        case f : NoSuccess => res.asInstanceOf[NoSuccess]
      }
    }
  }
  
  def strong[T](p: => Parser[T]): Parser[T] = new Parser[T]{
    def apply(in: Input)  = {
      val res = p(in)
      res match {
        case Success(out, in1) if res.next.atEnd  => res
        case Success(_, _) => Failure("<eof> expected", res.next)
        case f : NoSuccess => res.asInstanceOf[NoSuccess]
      }
    }
  }
}