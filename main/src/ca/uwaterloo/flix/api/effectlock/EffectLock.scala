package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Flix}
import ca.uwaterloo.flix.language.ast.{Scheme, Type, TypedAst}
import ca.uwaterloo.flix.util.Validation

import java.nio.file.{Files, Path}

object EffectLock {

  /**
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  private def getEffectLockFile(p: Path): Path = p.resolve("./effect.lock").normalize()

  def effectLock(path: Path, bootstrap: Bootstrap, flix: Flix): Validation[Unit, BootstrapError] = {
    // TODO: Refactor this to callee (Main) and pass the root and Flix instance to this function instead
    bootstrap.check(flix) match {
      case Validation.Failure(errors) => Validation.Failure(errors)
      case Validation.Success(root) =>
        val outputStream = Files.newOutputStream(getEffectLockFile(path))
        val signatures = mkEffectLockSignatures(root).mkString("\n") // TODO: make JSON
        Validation.Success(outputStream.write(signatures.getBytes))
    }
  }


  def mkEffectLockSignatures(root: TypedAst.Root): List[String] = {
    root.defs.map {
      // TODO: Consider types / type schemes and only reachable functions / visible functions
      case (sym, defn) => s"$sym:${defn.spec}"
    }.toList
  }

  def isSafe1(tpe1: Type, tpe2: Type): Boolean = ???

  def isSafe2(sc1: Scheme, sc2: Scheme): Boolean = ???
}
