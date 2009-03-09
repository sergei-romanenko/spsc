package spsc;

import org.junit.Test
import org.junit.Assert._
import TestUtils._

class SuperCompilerTest {
  
  @Test def processSamples(): Unit =
  {

    for (i <- 1 to 22) {
      if (i!=5)
      SuperCompilerApp.main(Array("-i", "input/test.sl",
          "-f", "test" + i,
          "-t", "output/test" + i + ".svg",
          "-p", "output/test" + i + ".sl"))
      print(i)
    }
  }
}
