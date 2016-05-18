package org.jetbrains.plugins.scala.annotator.quickfix

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

class MinimizeCodeQuickFixTest extends ScalaLightPlatformCodeInsightTestCaseAdapter {
  def doTest(sourceText: String, expectedText: String)(test: MinimizeCodeQuickFix => Unit) {
    configureFromFileTextAdapter("dummy.scala", sourceText)
    val scalaFile: ScalaFile = getFileAdapter.containingScalaFile.get
    val element =
      scalaFile.findElementAt(getEditorAdapter.getCaretModel.getOffset)
        .parents.collectFirst { case x: ScalaPsiElement => x }.get
    test(new MinimizeCodeQuickFix(element))
    checkResultByText(expectedText)
  }

  def testImplementationRemoval() = {
    val sourceText =
      """
        |def func1: Int = 2 + 2
        |def func2 = func1 * func1
        |val x1: Int = 2 + 2
        |val x2 = x1 * x1
        |def func3: Int = {
        |    val y1: Int = 2 + 2
        |    val y2 = y1 * y1
        |}
        |def func4 = {
        |    2 + 2
        |}
        |def func5: Int = {
        |    val y1: Int = 2 + 2
        |    val y2 = y1 * y1
        |    val func6: Boolean = {
        |        val y3: String = <caret>"foobar"
        |    }
        |}
      """.stripMargin.replace("\r", "").trim

    val expectedNoTypesInfer =
      """
        |def func1: Int = ???
        |def func2 = func1 * func1
        |val x1: Int = ???
        |val x2 = x1 * x1
        |def func3: Int = ???
        |def func4 = {
        |    2 + 2
        |}
        |def func5: Int = {
        |    val y1: Int = ???
        |    val y2 = y1 * y1
        |    val func6: Boolean = {
        |        val y3: String = "foobar"
        |    }
        |}
      """.stripMargin.replace("\r", "").trim

    val expectedWithTypesInfer =
      """
        |def func1: Int = ???
        |def func2: Int /*auto-inferred type*/ = ???
        |val x1: Int = ???
        |val x2: Int /*auto-inferred type*/ = ???
        |def func3: Int = ???
        |def func4: Int /*auto-inferred type*/ = ???
        |def func5: Int = {
        |    val y1: Int = ???
        |    val y2: Int /*auto-inferred type*/ = ???
        |    val func6: Boolean = {
        |        val y3: String = "foobar"
        |    }
        |}
      """.stripMargin.replace("\r", "").trim

    doTest(sourceText, expectedNoTypesInfer)(_.eraseTypedImplementations(getFileAdapter, autoInferTypes = false))
    doTest(sourceText, expectedWithTypesInfer)(_.eraseTypedImplementations(getFileAdapter, autoInferTypes = true))
  }
}
