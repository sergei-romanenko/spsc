package spsc;

import org.junit.Test
import org.junit.Assert._
import TestUtils._

class SuperCompilerTest {
  
  @Test def processSamples(): Unit =
  {
    
    SuperCompilerApp.main(Array("-i", "input/append.sl",
                                "-f", "append2", 
                                "-t", "output/append2.svg",
                                "-p", "output/append2.sl"))
    SuperCompilerApp.main(Array("-i", "input/append.sl",
                                "-f", "append3",
                                "-t", "output/append3.svg",
                                "-p", "output/append3.sl"))

    for (i <- 1 to 21) {
      SuperCompilerApp.main(Array("-i", "input/test.sl",
          "-f", "test" + i,
          "-t", "output/test" + i + ".svg",
          "-p", "output/test" + i + ".sl"))
    }
  }
}
