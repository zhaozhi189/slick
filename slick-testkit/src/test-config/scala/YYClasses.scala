package scala.slick.yy.test

import scala.slick.yy.VirtualizationMain

object YYClasses {
  def main(args: Array[String]) {
    val outputDir = args(0)
    VirtualizationMain.main(Array("scala.slick.yy.test.YYDefinitions", outputDir, "scala.slick.yy.VirtualizedCG"))
  }
}