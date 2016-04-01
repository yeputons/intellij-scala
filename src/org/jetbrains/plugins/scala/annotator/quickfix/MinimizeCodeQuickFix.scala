package org.jetbrains.plugins.scala
package annotator
package quickfix

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.codeInsight.intention.types.{AddOnlyStrategy, ToggleTypeAnnotation}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScExpression, ScTuple}
import org.jetbrains.plugins.scala.{DesktopUtils, ScalaBundle}
import org.jetbrains.plugins.scala.debugger.evaluation.ScalaCodeFragment
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScPattern, ScReferencePattern, ScTypedPattern}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.StdType

/**
  * Egor Suvorov
  */
class MinimizeCodeQuickFix(expr: ScalaPsiElement) extends IntentionAction {
  def getText: String = ScalaBundle.message("minimize.code.fix")

  def startInWriteAction: Boolean = true

  def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = {
    if (file.isInstanceOf[ScalaCodeFragment]) return false
    true
  }

  def minimizeSubtree(elem : PsiElement): Unit = {
    elem match {
      case (func : ScFunctionDefinition) =>
        val autoInferredType =
          if (func.returnTypeElement.isEmpty && func.hasAssign) {
            AddOnlyStrategy.addToFunction(func)
            true
          } else {
            false
          }
        func.returnTypeElement match {
          case Some(typeElement) =>
            val assign = func.findFirstChildByType(ScalaTokenTypes.tASSIGN)
            if (assign != null) {
              func.deleteChildRange(assign.getNextSibling, func.getLastChild)
              func.add(ScalaPsiElementFactory.parseElement("???", elem.getManager))
            }
          case None =>
            if (!func.hasAssign) {
              val params = func.getParameterList
              func.deleteChildRange(params.getNextSibling, func.getLastChild)
              ScalaPsiElementFactory.parseElements(": Unit = ???", elem.getManager)
                .foreach(func.add)
            }
        }
        if (autoInferredType) {
          func.add(ScalaPsiElementFactory.createBlockCommentFromText("auto-inferred type", elem.getManager))
        }
      case (pat : ScPatternDefinition) =>
        if (pat.isSimple) {
          if (pat.typeElement.isEmpty) {
            AddOnlyStrategy.addToValue(pat)
            if (pat.typeElement.isDefined) {
              pat.addAfter(ScalaPsiElementFactory.createBlockCommentFromText("auto-inferred type", elem.getManager), pat.typeElement.get)
            }
          }
          if (pat.typeElement.isDefined && pat.expr.isDefined) {
            pat.expr.get.replace(ScalaPsiElementFactory.parseElement("???", elem.getManager))
          }
        } else {
          for (binding <- pat.bindings) {
            binding match {
              case p: ScTypedPattern =>
              case p: ScReferencePattern =>
                if (!p.expectedType.contains(StdType.ANY)) {
                  val ty = p.expectedType.get
                  AddOnlyStrategy.addToPattern(binding)
                  binding.add(ScalaPsiElementFactory.createBlockCommentFromText("auto-inferred type", elem.getManager))
              }
              case _ =>
            }
          }
          pat.expr match {
            case Some(tuple : ScTuple) =>
              for ((binding, expr) <- pat.bindings.zip(tuple.exprs)) {
                binding match {
                  case typedBinding : ScTypedPattern =>
                    if (typedBinding.typePattern.isDefined) {
                      expr.replace(ScalaPsiElementFactory.parseElement("???", elem.getManager))
                    }
                  case _ =>
                }
              }
            case _ =>
          }
        }
      case _ =>
    }
    for (child <- elem.getChildren) {
      minimizeSubtree(child)
    }
  }

  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    for (elem <- List(expr) ++ expr.parentsInFile if elem.getParent != null) {
      for (child <- elem.getParent.getChildren if child != elem) {
        minimizeSubtree(child)
      }
    }
  }

  def getFamilyName: String = ScalaBundle.message("minimize.code.fix")
}
