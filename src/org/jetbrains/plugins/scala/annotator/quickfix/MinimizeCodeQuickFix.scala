package org.jetbrains.plugins.scala
package annotator
package quickfix

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.codeInsight.intention.types.AddOrRemoveStrategy
import org.jetbrains.plugins.scala.debugger.evaluation.ScalaCodeFragment
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDeclaration, ScFunctionDefinition, ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.{Any => AnyType, Nothing => NothingType, Unit => UnitType}

import scala.collection.mutable

/**
  * Egor Suvorov
  */
class MinimizeCodeQuickFix(rootElement: ScalaPsiElement) extends IntentionAction {
  def getText: String = ScalaBundle.message("minimize.code.fix")

  def startInWriteAction: Boolean = true

  def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = {
    if (file.isInstanceOf[ScalaCodeFragment]) return false
    true
  }

  def getFamilyName: String = ScalaBundle.message("minimize.code.fix")

  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    file.accept(getExprErasureVisitor(false))

    val referencesUsed = mutable.Set.empty[PsiNamedElement]
    val shouldNotRemove = mutable.Set.empty[PsiElement]
    def visitPrerequisites(expr: ScExpression): Unit = {
      expr.accept(new ScalaRecursiveElementVisitor() {
        override def visitElement(element: ScalaPsiElement): Unit = {
          shouldNotRemove += element
          super.visitElement(element)
        }

        override def visitReferenceExpression(ref: ScReferenceExpression): Unit = {
          ref.bind().map(_.getElement).foreach(reference => {
            if (referencesUsed.contains(reference)) {
              ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("processed", ref.getManager), ref)
              super.visitReferenceExpression(ref)
              return
            }
            if (reference.getContainingFile != file) {
              super.visitReferenceExpression(ref)
              return
            }
            referencesUsed += reference
            var current: PsiElement = reference
            while (current != null && !shouldNotRemove.contains(current)) {
              shouldNotRemove += current
              current = current.getParent
            }
            reference match {
              case param: ScParameter =>
                ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("param", ref.getManager), ref)
                param.add(ScalaPsiElementFactory.createBlockCommentFromText("used", param.getManager))
              case _: ScFunctionDefinition | _: ScFunctionDeclaration =>
                ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("func", ref.getManager), ref)
                reference.add(ScalaPsiElementFactory.createBlockCommentFromText("used", reference.getManager))
              case pat: ScPattern =>
                ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("pattern", ref.getManager), ref)
                pat.add(ScalaPsiElementFactory.createBlockCommentFromText("used", pat.getManager))
                pat.parentsInFile.filter(_.isInstanceOf[ScPatternDefinition]).take(1).foreach {
                  case patDef: ScPatternDefinition =>
                    decomposePatternDefinition(patDef) {
                      case (patInDef, exprInDef) if pat eq patInDef =>
                        exprInDef.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("init for " + pat.getName, pat.getManager), exprInDef.firstChild.get)
                        visitPrerequisites(exprInDef)
                    }
                  case _ =>
                }
              case _ =>
            }
          })
          super.visitReferenceExpression(ref)
        }
      })
    }
    val startingElement = rootElement.parentsInFile.find(_.isInstanceOf[ScBlockExpr]).map(_.asInstanceOf[ScBlockExpr])
    startingElement.foreach(e => {
      e.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("startingElement", e.getManager), e.firstChild.get)
      visitPrerequisites(e)
    })
    shouldNotRemove ++= rootElement.parents

    file.accept(new ScalaRecursiveElementVisitor() {
      override def visitFunctionDefinition(fun: ScFunctionDefinition): Unit = {
        if (fun.hasModifierPropertyScala("implicit")) {
          shouldNotRemove ++= fun.parents
        }
        super.visitFunctionDefinition(fun)
      }

      override def visitFunctionDeclaration(fun: ScFunctionDeclaration): Unit = {
        if (fun.hasModifierPropertyScala("implicit")) {
          shouldNotRemove ++= fun.parents
        }
        super.visitFunctionDeclaration(fun)
      }
    })

    file.accept(new ScalaRecursiveElementVisitor() {
      override def visitFunctionDefinition(fun: ScFunctionDefinition): Unit = {
        if (!shouldNotRemove.contains(fun)) {
          //fun.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("unused", fun.getManager), fun.paramClauses)
          fun.delete()
        }
        super.visitFunctionDefinition(fun)
      }

      override def visitFunctionDeclaration(fun: ScFunctionDeclaration): Unit = {
        if (!shouldNotRemove.contains(fun)) {
          //fun.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("unused", fun.getManager), fun.paramClauses)
          fun.delete()
        }
        super.visitFunctionDeclaration(fun)
      }

      override def visitExpression(expr: ScExpression): Unit = {
        if (!shouldNotRemove.contains(expr)) {
          if (expr.parent.exists({
            case _: ScTemplateBody => true
            case _: ScBlock => true
            case _ => false
          })) {
            expr.delete()
          }
        }
        super.visitExpression(expr)
      }

      override def visitPatternDefinition(patDef: ScPatternDefinition): Unit = {
        if (!patDef.hasModifierPropertyScala("implicit")) {
          decomposePatternDefinition(patDef) {
            case (pat: ScReferencePattern, expr) =>
              if (!shouldNotRemove.contains(pat) && !shouldNotRemove.contains(expr)) {
                pat.replace(ScalaPsiElementFactory.createWildcardPattern(pat.getManager))
                expr.replaceExpression(
                  ScalaPsiElementFactory.createExpressionWithContextFromText("???", expr.getContext, expr),
                  removeParenthesis = true)
              }
            case _ =>
          }
        }
        super.visitPatternDefinition(patDef)
      }
    })

    //file.accept(getExprErasureVisitor(true))
  }

  def getExprErasureVisitor(autoInferTypes: Boolean): ScalaRecursiveElementVisitor =
    new ScalaRecursiveElementVisitor {
      override def visitFunctionDefinition(fun: ScFunctionDefinition): Unit = {
        if (!fun.isAncestorOf(rootElement)) {
          if (autoInferTypes && !fun.hasExplicitType) {
            val expectedType = fun.returnType.getOrAny
            if (expectedType != AnyType && expectedType != NothingType) {
              AddOrRemoveStrategy.addToFunction(fun)
              addAutoInferredComment(fun)
            }
          }
          if (fun.hasExplicitType) {
            if (!fun.hasAssign) {
              fun.addBefore(ScalaPsiElementFactory.createAssign(fun.getManager), fun.body.get)
              fun.addBefore(ScalaPsiElementFactory.createWhitespace(fun.getManager), fun.body.get)
            }
            fun.body.foreach(_.replaceExpression(
              ScalaPsiElementFactory.createExpressionWithContextFromText("???", fun.getContext, fun),
              removeParenthesis = true))
          }
        }
        super.visitFunctionDefinition(fun)
      }

      override def visitPatternDefinition(patDef: ScPatternDefinition): Unit = {
        if (!patDef.isAncestorOf(rootElement)) {
          // We should not replace expressions with ??? as we go because it may break type inferrence.
          // E.g. if we replace expression in `val x, y = 5` after seeing `x` type for `y` will be impossible to infer.
          val exprsToReplace = mutable.Set.empty[ScExpression]
          decomposePatternDefinition(patDef) {
            case (_: ScTypedPattern, expr) =>
              exprsToReplace += expr
            case (p: ScReferencePattern, expr) =>
              if (p.expectedType.isDefined && autoInferTypes && !p.expectedType.contains(AnyType) && !p.expectedType.contains(NothingType)) {
                AddOrRemoveStrategy.addToPattern(p)
                addAutoInferredComment(p)
                exprsToReplace += expr
              }
            case _ =>
          }
          exprsToReplace.foreach(_.replace(ScalaPsiElementFactory.parseElement("???", rootElement.getManager)))
        }
        super.visitPatternDefinition(patDef)
      }
    }

  def decomposePatternDefinition(patDef: ScPatternDefinition)(visitor: (ScPattern, ScExpression) => Unit): Unit = {
    if (patDef.expr.isEmpty) {
      return
    }
    for (pattern <- patDef.pList.patterns) {
      decomposePatternAssignment(pattern, patDef.expr.get)(visitor)
    }
  }

  def decomposePatternAssignment(pattern: ScPattern, expr: ScExpression)(visitor: (ScPattern, ScExpression) => Unit): Unit = {
    visitor(pattern, expr)
    (pattern, expr) match {
      case (patTuple: ScTuplePattern, exprTuple: ScTuple)
        if patTuple.subpatterns.length == exprTuple.exprs.length
      =>
        for ((childPattern, childExpr) <- patTuple.subpatterns.zip(exprTuple.exprs)) {
          decomposePatternAssignment(childPattern, childExpr)(visitor)
        }
      case _ =>
    }
  }

  def addAutoInferredComment(fun: ScFunctionDefinition): Unit = {
    fun.returnTypeElement match {
      case Some(anchor) =>
        addAutoInferredComment(fun, anchor)
      case None =>
    }
  }

  def addAutoInferredComment(binding: ScBindingPattern): Unit = {
    // During PSI-tree modification type element can be already added but not embedded into ScReferencePattern
    // and therefore it can be not ScTypedPattern
    val anchor = binding match {
      case typed: ScTypedPattern => typed.typePattern
      case _ => binding.nextSiblings.filter(_.isInstanceOf[ScTypeElement]).find(_ => true)
    }
    if (anchor.isDefined) {
      addAutoInferredComment(binding, anchor.get)
    }
  }

  def addAutoInferredComment(element: PsiElement, addAfter: PsiElement): Unit = {
    element.addAfter(ScalaPsiElementFactory.createBlockCommentFromText("auto-inferred type", element.getManager), addAfter)
  }
}
