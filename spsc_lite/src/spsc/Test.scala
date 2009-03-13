package spsc

object Test {
  def main(args : Array[String]) : Unit = {
    val v = Variable("zzzz")
    println(v.productPrefix)
    println(v)
    val z: Pair[_,_] = (1,2)
    println(z)
  }
}
