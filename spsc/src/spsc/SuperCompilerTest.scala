package spsc;

import org.junit.Test
import org.junit.Assert._
import SmallLanguage._
import TestUtils._

class SuperCompilerTest {
  
  @Test def processExamples(): Unit =
  {
    
    SuperCompilerApp.main(Array("-i", "examples/append.sl",
                                "-f", "append2", 
                                "-t", "build/test_results/append2.svg",
                                "-p", "build/test_results/append2.sl"))
    SuperCompilerApp.main(Array("-i", "examples/append.sl",
                                "-f", "append3",
                                "-t", "build/test_results/append3.svg",
                                "-p", "build/test_results/append3.sl"))

    for (i <- 1 to 21) {
      SuperCompilerApp.main(Array("-i", "examples/test.sl",
          "-f", "test" + i,
          "-t", "build/test_results/test" + i + ".svg",
          "-p", "build/test_results/test" + i + ".sl"))
    }
  }
}
