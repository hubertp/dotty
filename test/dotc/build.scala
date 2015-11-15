package dotc

import java.io.File

object build extends tests {

  private def removeFiles(file: File, deleteDir: Boolean = false): Unit = {
    val files = file.listFiles()
    // Some JVMs return null for empty dirs
    if (files != null)
      files.foreach { f =>
        if (f.isDirectory)  removeFiles(f, deleteDir = true)
        else f.delete()
      }
    if (deleteDir) file.delete()
  }

  private val keepDirSuffix = ".keep"

  def main(args: Array[String]): Unit = {
    println("------------  Building dotty  ------------")
    removeFiles(new File(defaultOutputDir)) // clear previous output

    val keepFile = new File(defaultOutputDir + keepDirSuffix)
    keepFile.createNewFile()

    dotty // build output dir

    val execArgs = "jar" :: "cf" :: "dotty.jar" :: "-C" :: "out" :: "." :: Nil
    val exec =
      Runtime.getRuntime.exec(execArgs.toArray)
    exec.waitFor()
    //exec
  }
}
