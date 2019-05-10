package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType}
import ca.uwaterloo.flix.language.phase.jvm.{JvmName, NamespaceInfo, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes.TagClass
import ca.uwaterloo.flix.language.phase.njvm.interfaces.ContinuationInterface

object GenTagClasses extends MnemonicsGenerator{
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param types  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo], ns: Set[NamespaceInfo])(implicit root: FinalAst.Root, flix: Flix): Map[JvmName, Mnemonics.MnemonicsClass] = {
    tags.foldLeft(map) {
      case (macc, tag) =>
        macc + (getErasedJvmType(tag.tagType) match {
          case PrimBool => new TagClass[MBool](tag).getClassMapping
          case PrimChar =>  new TagClass[MChar](tag).getClassMapping
          case PrimByte =>  new TagClass[MByte](tag).getClassMapping
          case PrimShort =>  new TagClass[MShort](tag).getClassMapping
          case PrimInt =>  new TagClass[MInt](tag).getClassMapping
          case PrimLong =>  new TagClass[MLong](tag).getClassMapping
          case PrimFloat =>  new TagClass[MFloat](tag).getClassMapping
          case PrimDouble =>  new TagClass[MDouble](tag).getClassMapping
          case Reference(_) => new TagClass[Ref[MObject]](tag).getClassMapping
          case _ => ???
        })
    }

    map
  }
}
