/* NOTE: Adapted from ScalaJSPartest.scala in
 * https://github.com/scala-js/scala-js/
 * TODO make partest configurable */

package dotty.partest

import scala.reflect.io.AbstractFile
import scala.tools.partest._
import scala.tools.partest.nest._
import scala.util.matching.Regex
import tools.nsc.io.{ File => NSCFile }
import java.io.{ File, PrintStream, FileOutputStream, PrintWriter, FileWriter }
import java.net.URLClassLoader

/** Runs dotty partest from the Console, discovering test sources in
  * DPConfig.testRoot that have been generated automatically by
  * DPPrepJUnitRunner. Use `sbt partest` to run. If additional jars are
  * required by some run tests, add them to partestDeps in the sbt Build.scala.
  */
object DPConsoleRunner {

  lazy val infoMsg =
    "Error: DPConsoleRunner needs \"-dottyJars <jarCount> <jars>*\"."

  def invalidCountMsg(jarsCount: List[String], expected: Int) =
    s"Error: DPConsoleRunner found wrong number of dottyJars: ${jarsCount}," +
    s"expected: ${expected}"

  def invalidJarsMsg(opts: List[String]) =
    s"Error: DPConsoleRunner found several -dottyJars options: ${opts}"

  def main(args: Array[String]): Unit = {
    // unfortunately sbt runTask passes args as single string
    // extra jars for run tests are passed with
    // -dottyJars <count> <jar1> <jar2> ...
    val JarFinder = """-dottyJars (\d*) (.*)""".r
    val (jarList, otherArgs) =
      args.toList.partition(JarFinder.findFirstIn(_).isDefined)
    val (extraJars, moreArgs) = jarList match {
      case Nil                              =>
        sys.error(infoMsg)
      case JarFinder(count0, jarString) :: Nil  =>
        val jars = jarString.split(" ").toList
        val count = count0.toInt
        if (jars.length < count)  sys.error(invalidCountMsg(jars, count))
        else                      jars.splitAt(count)
      case list =>
        sys.error(invalidJarsMsg(list))
    }
    new DPConsoleRunner(
      (otherArgs ::: moreArgs) mkString (" "), extraJars).runPartest
  }
}

// console runner has a suite runner which creates a test runner for each test
class DPConsoleRunner(args: String, extraJars: List[String])
  extends ConsoleRunner(args) {

  override val suiteRunner = new DPSuiteRunner (
    testSourcePath  = optSourcePath getOrElse DPConfig.testRoot,
    fileManager     = new DottyFileManager(extraJars),
    updateCheck     = optUpdateCheck,
    failed          = optFailed,
    consoleArgs     = args)

  override def run = {}
  def runPartest = super.run
}

class DottyFileManager(extraJars: List[String]) extends FileManager(Nil) {

  lazy val extraJarList = extraJars.map(NSCFile(_))

  private def libs(name: String) =
    Path(extraJars.find(_.contains(name)).getOrElse(""))

  override lazy val libraryUnderTest  = libs("scala-library")

  override lazy val reflectUnderTest  = libs("scala-reflect")

  override lazy val compilerUnderTest = libs("dotty")

}

object DPSuiteRunner {

  import NestUI.color

  val differ = color.bold(color.red("% ")) + "diff "

}

class DPSuiteRunner(
  testSourcePath: String, // relative path, like "files", or "pending"
  fileManager: DottyFileManager,
  updateCheck: Boolean,
  failed: Boolean,
  consoleArgs: String,
  javaCmdPath: String = PartestDefaults.javaCmd,
  javacCmdPath: String = PartestDefaults.javacCmd,
  scalacExtraArgs: Seq[String] = Seq.empty)
extends SuiteRunner(testSourcePath, fileManager, updateCheck,
                    failed, javaCmdPath, javacCmdPath, scalacExtraArgs) {

  if (!DPConfig.runTestsInParallel)
    sys.props("partest.threads") = "1"

  sys.props("partest.root") = "."

  // override to provide Dotty banner
  override def banner: String = {
    s"""|Welcome to Partest for Dotty! Partest version: ${Properties.versionNumberString}
        |Compiler under test: dotty.tools.dotc.Bench or dotty.tools.dotc.Main
        |Generated test sources: ${PathSettings.srcDir}${File.separator}
        |Test directories: ${DPConfig.testDirs.toList.mkString(", ")}
        |Debugging: failed tests have compiler output in test-kind.clog, run output in test-kind.log, class files in test-kind.obj
        |Parallel: ${DPConfig.runTestsInParallel}
        |Options: (use partest --help for usage information) ${consoleArgs}
    """.stripMargin
  }

  // override for DPTestRunner and redirecting compilation output to test.clog
  override def runTest(testFile: File): TestState = {
    val runner = new DPTestRunner(testFile, this)

    val state =
      try {
        runner.run match {
          // Append compiler output to transcript if compilation failed,
          // printed with --verbose option
          case TestState.Fail(f, r@"compilation failed", transcript) =>
            TestState.Fail(f, r,
              transcript ++ runner.cLogFile.fileLines.dropWhile(_ == ""))
          case res => res
        }
      } catch {
        case t: Throwable =>
          throw new RuntimeException(s"Error running $testFile", t)
      }

    reportTest(state)
    runner.cleanup()

    onFinishTest(testFile, state)
  }

  // override NestUI.reportTest because --show-diff doesn't work.
  // The diff used seems to add each line to transcript separately,
  // whereas NestUI assumes that the diff string was added as one entry in
  // the transcript
  def reportTest(state: TestState) = {
    import NestUI._
    import DPSuiteRunner._

    if (isTerse && state.isOk) NestUI.reportTest(state)
    else {
      echo(statusLine(state))
      if (!state.isOk && isDiffy) {
        state.transcript.dropWhile(s => !(s startsWith differ)).
          foreach (echo(_))
      }
    }
  }
}

object DPTestRunner {

  private val logSuffix = "clog"

  private val nerrSuffix = "nerr"

  private val checkSrcSuffix = "checksrc"

  private val failedState = "compilation failed"

  private def nerrNumberMismatchMsg(exp: String, found: String) =
    s"wrong number of compilation errors, expected: $exp, found: $found"

  private val nerrOutputDiffMsg = "output differs"

  private val nerrExpectedErrorMsg = "expected compilation failure"

  val NerrFinder = """compilation failed with (\d+) errors""".r

  private sealed abstract class NegTestState {
    def state: Int
  }
  // Don't get confused, the neg test passes when compilation fails for at
  // least one round (optionally checking the number of compiler errors and
  // compiler console output)
  private case object CompFailed extends NegTestState {
    def state = 1
  }
  // the neg test fails when all rounds return either of these:
  private case class CompFailedButWrongNErr(expected: String, found: String)
    extends NegTestState { def state = CompFailedButWrongNErr.state }
  private case object CompFailedButWrongNErr {
    def state = 2
  }


  private case object CompFailedButWrongDiff extends NegTestState {
    def state = 3
  }

  private case object CompSucceeded extends NegTestState {
    def state = 0
  }

}

class DPTestRunner(testFile: File, suiteRunner: DPSuiteRunner)
  extends nest.Runner(testFile, suiteRunner) {

  import DPTestRunner._

  val cLogFile = SFile(logFile).changeExtension(logSuffix)

  // override to provide DottyCompiler
  override def newCompiler = new dotty.partest.DPDirectCompiler(this)

  // override to provide default dotty flags from file in directory
  override def flagsForCompilation(sources: List[File]): List[String] = {
    val specificFlags = super.flagsForCompilation(sources)
    if (specificFlags.isEmpty)  defaultFlags.getOrElse(Nil)
    else                        specificFlags
  }

  private lazy val defaultFlags =
    for {
        flagsFile <- parentFile.listFiles.toList.find(
                      _.getName == "__defaultFlags.flags")
        content   <- SFile(flagsFile).safeSlurp
    } yield words(content).filter(_.nonEmpty)

  // override to add the check for a number of compilation errors if there's a
  // target.nerr file
  override def runNegTest() = runInContext {
    import TestState.{ Crash, Fail }
    import scala.reflect.internal.FatalError

    def nerrIsOk(reason: String) = reason match {
      case NerrFinder(found)  =>
        SFile(FileOps(testFile) changeExtension nerrSuffix).safeSlurp match {
          case Some(exp) if (exp != found)  =>
            CompFailedButWrongNErr(exp, found)
          case _                            =>
            CompFailed
        }
      case _                  =>
        CompFailed
    }

    // we keep the partest semantics where only one round needs to fail
    // compilation, not all
    val compFailingRounds = compilationRounds(testFile).flatMap { round =>
      val ok = round.isOk
      setLastState(if (ok) genPass else genFail(failedState))
      if (!ok) Some(round.result) else None
    }

    val failureStates = compFailingRounds map {
      // Let it crash with a checkfile
      case Crash(_, t, _) if !checkFile.canRead ||
                             !t.isInstanceOf[FatalError] =>
        CompSucceeded
      case Fail(_, reason, _) =>
        if (diffIsOk) nerrIsOk(reason) else CompFailedButWrongDiff
      case _                  =>
        if (diffIsOk) CompFailed else CompFailedButWrongDiff
    }

    val results = failureStates.groupBy(elem => elem.state)

    if (results.isDefinedAt(CompFailed.state)) true
    else if (results.isDefinedAt(CompFailedButWrongNErr.state))
      results(CompFailedButWrongNErr.state).headOption map {
        case CompFailedButWrongNErr(exp, found) =>
          nextTestActionFailing(nerrNumberMismatchMsg(exp, found))
          true
        case _ =>
          false // keep pattern matcher quiet
      } get
    else if (results.isDefinedAt(CompFailedButWrongDiff.state)) {
      nextTestActionFailing(nerrOutputDiffMsg); true
    } else {
      nextTestActionFailing(nerrExpectedErrorMsg); false
    }

  }

  // override to change check file updating to original file, not generated
  override def diffIsOk: Boolean = {
    // always normalize the log first
    normalizeLog()
    val diff = currentDiff
    // if diff is not empty, is update needed?
    val updating: Option[Boolean] = (
      if (diff == "") None
      else Some(suiteRunner.updateCheck)
    )
    pushTranscript(s"diff $logFile $checkFile")

    nextTestAction(updating) {
      case Some(true) =>
        val origCheck = SFile(checkFile.changeExtension(checkSrcSuffix).fileLines(1))
        NestUI.echo(s"Updating original checkfile ${origCheck}")
        origCheck writeAll file2String(logFile)
        genUpdated()
      case Some(false) =>
        // Get a word-highlighted diff from git if we can find it
        val bestDiff =
          if (updating.isEmpty) ""
          else if (checkFile.canRead)
            gitDiff(logFile, checkFile) getOrElse {
              s"diff $logFile $checkFile\n$diff"
            }
          else diff
        pushTranscript(bestDiff)
        genFail(nerrOutputDiffMsg)
      case None        =>
        genPass()  // redundant default case
    } getOrElse true
  }

  // override because Dotty currently doesn't handle separate compilation well,
  // so we ignore groups (tests suffixed with _1 and _2)
  override def groupedFiles(sources: List[File]): List[List[File]] = {
    val grouped = sources groupBy (_.group)
    val flatGroup =
      List(grouped.keys.toList.sorted.
        map( k => grouped(k) sortBy (_.getName) ).flatten)

    try { // try/catch because of a bug in partest that throws exception
      if (flatGroup != super.groupedFiles(sources))
        throw new java.lang.UnsupportedOperationException()
    } catch {
      case e: java.lang.UnsupportedOperationException =>
        val genlogFWriter = new FileWriter(DPConfig.genLog.jfile, true)
        val genlogWriter = new PrintWriter(genlogFWriter, true)
        genlogWriter.println(
          s"""|Warning:
              |Overriding compilation groups for tests: $sources""".
            stripMargin)
        genlogWriter.close
        genlogFWriter.close
    }
    flatGroup
  }

  // override to avoid separate compilation of scala and java sources
  override def mixedCompileGroup(allFiles: List[File]): List[CompileRound] =
    List(OnlyDotty(allFiles))

  private case class OnlyDotty(fs: List[File]) extends CompileRound {
    def description = s"dotc $fsString"
    lazy val result = { pushTranscript(description) ; attemptCompile(fs) }
  }

  // override to add dotty and scala jars to classpath
  override def extraClasspath =
    suiteRunner.fileManager.asInstanceOf[DottyFileManager].extraJarList :::
      super.extraClasspath

  // override to keep class files if failed and delete clog if ok
  override def cleanup = if (lastState.isOk) {
    logFile.delete
    cLogFile.delete
    Directory(outDir).deleteRecursively
  }

}
