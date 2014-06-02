package spsc

import org.junit.Test

class SuperCompilerTest {
  
  @Test def processSamples(): Unit =
  {
    
    SuperCompilerApp.main(Array("-i", "samples/test/append.sl",
                                "-f", "append2", 
                                "-t", "samples/test/append2.out.svg",
                                "-p", "samples/test/append2.out.sl"))
    SuperCompilerApp.main(Array("-i", "samples/test/append.sl",
                                "-f", "append3",
                                "-t", "samples/test/append3.out.svg",
                                "-p", "samples/test/append3.out.sl"))

    for (i <- 1 to 21) {
      SuperCompilerApp.main(Array("-i", "samples/test/test.sl",
          "-f", "test" + i,
          "-t", "samples/test/test" + i + ".out.svg",
          "-p", "samples/test/test" + i + ".out.sl"))
    }
  }
}
