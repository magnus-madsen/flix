package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TraitCompletion
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Trait
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object TraitCompleter {

  /**
   * Returns a List of Completion for traits.
   * Whether the returned completions are qualified is based on whether the name in the error is qualified.
   * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
   */
  def getCompletions(err: ResolutionError.UndefinedTrait, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, namespace, ident)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, namespace: List[String], ident: String)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (namespace.nonEmpty)
      root.traits.values.collect{
        case trt if matchesTrait(trt, namespace, ident, uri, qualified = true) =>
          TraitCompletion(trt, ap, qualified = true, inScope = true)
      }
    else
      root.traits.values.collect({
        case trt if matchesTrait(trt, namespace, ident, uri, qualified = false) =>
          TraitCompletion(trt, ap, qualified = false, inScope = inScope(trt, env))
      })
  }

  /**
   * Checks if the definition is in scope.
   * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
   */
  private def inScope(struct: TypedAst.Trait, scope: LocalScope): Boolean = {
    val thisName = struct.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Trait(_, _, _, thatName, _, _, _, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = struct.sym.namespace.isEmpty
    isRoot || isResolved
  }

  /**
   * Checks if the definition matches the QName.
   * Names should match and the definition should be available.
   */
  private def matchesTrait(struct: TypedAst.Trait, namespace: List[String], ident: String, uri: String, qualified: Boolean): Boolean = {
    val isPublic = struct.mod.isPublic && !struct.ann.isInternal
    val isInFile = struct.sym.loc.source.name == uri
    val isMatch = if (qualified)
      CompletionUtils.matchesQualifiedName(struct.sym.namespace, struct.sym.name, namespace, ident)
    else
      CompletionUtils.fuzzyMatch(ident, struct.sym.name)
    isMatch && (isPublic || isInFile)
  }
}
