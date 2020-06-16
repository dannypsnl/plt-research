import lc.cps._
import lc.lang._

object Main extends App {
  var testRealLangExtend = Application(Variable("g"), List(Variable("a")))
  // with continuation: 'halt
  println(RealLangM.transform_c(testRealLangExtend, AVar("halt")))
  // (g a halt)
  testRealLangExtend = Application(
    CallWithCurrentContinuation(),
    List(Lambda(
      List("halt"),
      Application(Variable("halt"), List(LiteralInt(5)))
    ))
  )
  println(RealLangM.transform_c(testRealLangExtend, AVar("halt")))
  // ((lambda (f cc) (f (lambda (x _) (cc x)) cc)) ;; Here is call/cc
  //   (lambda (halt $k1) (halt 5 $k1)) ;; our f
  //   halt ;; top level cc
  // )
}
