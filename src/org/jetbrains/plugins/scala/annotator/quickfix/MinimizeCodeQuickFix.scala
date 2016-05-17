package org.jetbrains.plugins.scala
package annotator
package quickfix

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi._
import org.jetbrains.plugins.scala.codeInsight.intention.types.AddOrRemoveStrategy
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.debugger.evaluation.ScalaCodeFragment
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDeclaration, ScFunctionDefinition, ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.impl.base.patterns.ScTypedPatternImpl
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext
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

  val debugComments: Boolean = false
  val removeFunctions: Boolean = false

  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    eraseTypedImplementations(file, autoInferTypes = false)

    val referencesUsed = mutable.Set.empty[PsiNamedElement]
    val shouldNotRemove = mutable.Set.empty[PsiElement]
    def visitPrerequisites(expr: ScalaPsiElement): Unit = {
      expr.accept(new ScalaRecursiveElementVisitor() {
        override def visitElement(element: ScalaPsiElement): Unit = {
          val parentsToRemove = element.parentsWithSelfInFile.takeWhile(!shouldNotRemove.contains(_))
          shouldNotRemove ++= parentsToRemove
          super.visitElement(element)
        }

        override def visitReferenceExpression(ref: ScReferenceExpression): Unit = {
          ref.bind().map(_.getElement).foreach(reference => {
            if (referencesUsed.contains(reference)) {
              super.visitReferenceExpression(ref)
              return
            }
            if (reference.getContainingFile != file) {
              super.visitReferenceExpression(ref)
              return
            }
            referencesUsed += reference
            val parentsToRemove = reference.parentsWithSelfInFile.takeWhile(!shouldNotRemove.contains(_))
            shouldNotRemove ++= parentsToRemove
            reference match {
              case param: ScParameter =>
                if (debugComments) {
                  ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("param", ref.getManager), ref)
                  param.add(ScalaPsiElementFactory.createBlockCommentFromText("used", param.getManager))
              }
              case _: ScFunctionDefinition | _: ScFunctionDeclaration =>
                if (debugComments) {
                  ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("func", ref.getManager), ref)
                  reference.add(ScalaPsiElementFactory.createBlockCommentFromText("used", reference.getManager))
                }
              case pat: ScPattern =>
                if (debugComments) {
                  ref.getParent.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("pattern", ref.getManager), ref)
                  pat.add(ScalaPsiElementFactory.createBlockCommentFromText("used", pat.getManager))
                }
                pat.parentsInFile.collect { case pat: ScPatternDefinition => pat }.take(1).foreach(patDef => {
                  if (debugComments) {
                    patDef.addAfter(ScalaPsiElementFactory.createBlockCommentFromText("init for " + pat.getName, pat.getManager), patDef.findFirstChildByType(ScalaTokenTypes.tASSIGN))
                  }
                  patDef.expr.foreach(visitPrerequisites(_))
                })
              case _ =>
            }
          })
          super.visitReferenceExpression(ref)
        }
      })
    }
    val startingElement = rootElement.parentsInFile.collect { case expr: ScBlockStatement => expr }.take(1)
    startingElement.foreach(e => {
      if (debugComments) {
        e.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("startingElement", e.getManager), e.firstChild.get)
      }
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
          if (removeFunctions) {
            fun.delete()
          } else if (debugComments) {
            fun.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("unused", fun.getManager), fun.paramClauses)
          }
        }
        super.visitFunctionDefinition(fun)
      }

      override def visitFunctionDeclaration(fun: ScFunctionDeclaration): Unit = {
        if (!shouldNotRemove.contains(fun)) {
          if (removeFunctions) {
            fun.delete()
          } else if (debugComments) {
            fun.addBefore(ScalaPsiElementFactory.createBlockCommentFromText("unused", fun.getManager), fun.paramClauses)
          }
        }
        super.visitFunctionDeclaration(fun)
      }

      override def visitExpression(expr: ScExpression): Unit = {
        if (!shouldNotRemove.contains(expr)) {
          if (expr.parent.exists {
            case _: ScTemplateBody => true
            case _: ScBlock => true
            case _ => false
          }) {
            expr.replaceExpression(
              ScalaPsiElementFactory.createExpressionWithContextFromText("???", expr.getContext, expr),
              removeParenthesis = true
            )
          }
        }
        super.visitExpression(expr)
      }

      override def visitPatternDefinition(patDef: ScPatternDefinition): Unit = {
        if (!patDef.hasModifierPropertyScala("implicit")) {
          if (!shouldNotRemove.contains(patDef) && !patDef.expr.exists(shouldNotRemove.contains(_))) {
            patDef.delete()
          }
        }
        super.visitPatternDefinition(patDef)
      }
    })

    file.accept(new ScalaRecursiveElementVisitor() {
      override def visitReferenceExpression(ref: ScReferenceExpression): Unit = {
        if (ref.getReference.refName == "???") {
          if (ref.nextSiblings.exists(_.isInstanceOf[ScExpression])) {
            ref.delete()
          }
        }
        super.visitReferenceExpression(ref)
      }
    })
    //file.accept(getExprErasureVisitor(true))
  }

  def eraseTypedImplementations(file: PsiFile, autoInferTypes: Boolean): Unit =
    file.accept(new ScalaRecursiveElementVisitor {
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
          patDef.typeElement match {
            case Some(_) =>
              exprsToReplace ++= patDef.expr
            case None if autoInferTypes =>
              val expectedType = patDef.getType(TypingContext.empty).getOrNothing
              if (expectedType != AnyType && expectedType != NothingType) {
                AddOrRemoveStrategy.addToValue(patDef)
                addAutoInferredComment(patDef)
                exprsToReplace ++= patDef.expr
              }
            case _ =>
          }
          exprsToReplace.foreach(_.replace(ScalaPsiElementFactory.parseElement("???", rootElement.getManager)))
        }
        super.visitPatternDefinition(patDef)
      }
    })

  def addAutoInferredComment(fun: ScFunctionDefinition): Unit = {
    fun.returnTypeElement.foreach(addAutoInferredComment(fun, _))
  }

  def addAutoInferredComment(patDef: ScPatternDefinition): Unit = {
    patDef.typeElement.foreach(addAutoInferredComment(patDef, _))
  }

  def addAutoInferredComment(element: PsiElement, addAfter: PsiElement): Unit = {
    element.addAfter(ScalaPsiElementFactory.createBlockCommentFromText("auto-inferred type", element.getManager), addAfter)
  }
}
