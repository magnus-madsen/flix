package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EffectCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Effect
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object EffectCompleter {
  /**
    * Returns a List of Completion for effects.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
    */
  def getCompletions(err: ResolutionError.UndefinedType, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, namespace, ident)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (namespace.nonEmpty)
      root.effects.values.collect{
        case effect if matchesEffect(effect, namespace, ident, uri, qualified = true) =>
          EffectCompletion(effect, ap, qualified = true, inScope = true)
      }
    else
      root.effects.values.collect({
        case effect if matchesEffect(effect, namespace, ident, uri, qualified = false) =>
          EffectCompletion(effect, ap, qualified = false, inScope = inScope(effect, env))
      })
  }

  /**
    * Checks if the definition is in scope.
    * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
    */
  private def inScope(effect: TypedAst.Effect, scope: LocalScope): Boolean = {
    val thisName = effect.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Effect(_, _, _, thatName, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = effect.sym.namespace.isEmpty
    isRoot || isResolved
  }

  /**
    * Checks if the definition matches the QName.
    * Names should match and the definition should be available.
    */
  private def matchesEffect(effect: TypedAst.Effect, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean = {
    val isPublic = effect.mod.isPublic && !effect.ann.isInternal
    val isInFile = effect.sym.loc.source.name == uri
    val isMatch = if (qualified) {
      CompletionUtils.matchesQualifiedName(effect.sym.namespace, effect.sym.name, namespace, ident)
    } else
      CompletionUtils.fuzzyMatch(ident, effect.sym.name)
    isMatch && (isPublic || isInFile)
  }
}