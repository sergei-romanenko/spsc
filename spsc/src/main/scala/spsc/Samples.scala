package spsc

object Samples extends App {
  SuperCompilerApp.main(Array(
    "-i", "samples/success/append.sl",
    "-f", "appendXYaZ",
    "-t", "samples/success/appendXYaZ.out.svg",
    "-p", "samples/success/appendXYaZ.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/append.sl",
    "-f", "appendXYaX",
    "-t", "samples/success/appendXYaX.out.svg",
    "-p", "samples/success/appendXYaX.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/append.sl",
    "-f", "appendXaYZ",
    "-t", "samples/success/appendXaYZ.out.svg",
    "-p", "samples/success/appendXaYZ.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/fail/takenm.sl",
    "-f", "takenm",
    "-t", "samples/fail/takenm.out.svg",
    "-p", "samples/fail/takenm.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/fail/mapfrom.sl",
    "-f", "mapfrom",
    "-t", "samples/fail/mapfrom.out.svg",
    "-p", "samples/fail/mapfrom.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/abab.sl",
    "-f", "abab",
    "-t", "samples/success/abab.out.svg",
    "-p", "samples/success/abab.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/fail/reverse.sl",
    "-f", "reverse",
    "-t", "samples/fail/reverse.out.svg",
    "-p", "samples/fail/reverse.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/eq.sl",
    "-f", "eqxx",
    "-t", "samples/success/eqxx.out.svg",
    "-p", "samples/success/eqxx.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/eq.sl",
    "-f", "eqxSx",
    "-t", "samples/success/eqxSx.out.svg",
    "-p", "samples/success/eqxSx.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/eq.sl",
    "-f", "eqSxx",
    "-t", "samples/success/eqSxx.out.svg",
    "-p", "samples/success/eqSxx.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/eq.sl",
    "-f", "eqSZx",
    "-t", "samples/success/eqSZx.out.svg",
    "-p", "samples/success/eqSZx.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/mapnot.sl",
    "-f", "mapNotNot",
    "-t", "samples/success/mapNotNot.out.svg",
    "-p", "samples/success/mapNotNot.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/mapnot.sl",
    "-f", "mapNotNotNot",
    "-t", "samples/success/mapNotNotNot.out.svg",
    "-p", "samples/success/mapNotNotNot.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/member.sl",
    "-f", "member",
    "-t", "samples/success/member.out.svg",
    "-p", "samples/success/member.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/member.sl",
    "-f", "memberSZ",
    "-t", "samples/success/memberSZ.out.svg",
    "-p", "samples/success/memberSZ.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/member.sl",
    "-f", "memberZSZ",
    "-t", "samples/success/memberZSZ.out.svg",
    "-p", "samples/success/memberZSZ.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/fail/memberLazy.sl",
    "-f", "member",
    "-t", "samples/fail/memberLazy.out.svg",
    "-p", "samples/fail/memberLazy.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/notornot.sl",
    "-f", "notornot",
    "-t", "samples/success/notornot.out.svg",
    "-p", "samples/success/notornot.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/flip.sl",
    "-f", "flip2",
    "-t", "samples/success/flip2.out.svg",
    "-p", "samples/success/flip2.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/flip.sl",
    "-f", "flip3",
    "-t", "samples/success/flip3.out.svg",
    "-p", "samples/success/flip3.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/success/add.sl",
    "-f", "e",
    "-t", "samples/success/add.out.svg",
    "-p", "samples/success/add.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/input/kmpNestedCalls.sl",
    "-f", "matchAAB",
    "-t", "samples/input/kmpNestedCalls.out.svg",
    "-p", "samples/input/kmpNestedCalls.out.sl"
  ))

  SuperCompilerApp.main(Array(
    "-i", "samples/input/kmpTailCalls.sl",
    "-f", "matchAAB",
    "-t", "samples/input/kmpTailCalls.out.svg",
    "-p", "samples/input/kmpTailCalls.out.sl"
  ))
}
