package spsc;

import org.junit.Test
import org.junit.Assert._
import TestUtils._

class SuperCompilerTest {
  
  @Test def processSamples(): Unit =
  {
    // 17 is important
    val samples = List(1,2,3,6, 8, 9, 10, 11, 12, 17, 18, 20, 21)
    
    for (i <- samples) {
      SuperCompilerApp.main(Array("-i", "input/test.sl",
          "-f", "fTest" + i,
          "-t", "output/test" + i + ".svg",
          "-p", "output/test" + i + ".sl"))
      print(i)
    }
  }
}
