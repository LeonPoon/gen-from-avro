/*
 * Copyright 2023 Leon Poon
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.codepowered.avro_based.avsc_gen_scala

import com.codepowered.avro_based.avsc_gen_scala.AvscGenScala._
import org.apache.avro.Schema
import treehugger.forest._
import definitions._
import treehuggerDSL._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IterableHasAsScala


case class UnitInfo(
                     packageName: Option[String],
                     className: String
                   ) {
  def full: String = List(packageName, Some(className)).flatten.mkString(".")
}

case class GeneratedUnit(
                          generatedElement: UnitInfo,
                          fileContent: Tree
                        )

case class GeneratorSettings()


case class Generation(generatedUnits: List[GeneratedUnit], pendingGen: List[Gen])

object NoGeneration extends Generation(List.empty, List.empty)

trait Gen {
  def imports: Set[String] = Set()

  def putTree(parent: Gen, field: Schema.Field, i: Int, valueParamName: String): Tree = BLOCK(REF(field.name()) := REF(valueParamName) AS rootClass)

  def getTree(parent: Gen, field: Schema.Field, i: Int): Tree = REF(field.name())

  val schema: Schema

  def apply(): Generation = NoGeneration

  val canBeToplevel: Boolean = false

  val isPrimitive: Boolean

  val isComplex: Boolean = !isPrimitive

  def generatedUnitInfo: Option[UnitInfo] = None

  def rootClass: Type

  def defaultValue: Tree

  val schemaTreeBasedOnParent: Tree

  def optionType: Option[Type] = None
}

trait GenComplex extends Gen {
  override val isPrimitive: Boolean = false
  override val isComplex: Boolean = true
}

trait GenPrimitive extends Gen {
  override val isPrimitive: Boolean = true
  override val isComplex: Boolean = false

}

trait TopLevelGen extends GenComplex {

  override val canBeToplevel: Boolean = true

  override def apply(): Generation = Generation(
    List(GeneratedUnit(
      unitInfo,
      fileContent
    )),

    pendingGens
  )

  val unitInfo: UnitInfo

  override def generatedUnitInfo = Some(unitInfo)

  def fileContent: Tree

  def pendingGens: List[Gen]

}

object AvscGenScala {

  val SpecificRecordBaseClass: Type = RootClass.newClass("org.apache.avro.specific.SpecificRecordBase")
  val SchemaClass: Type = RootClass.newClass("org.apache.avro.Schema")
  val SchemaParserClass: Type = RootClass.newClass("org.apache.avro.Schema.Parser")
  val GenericEnumSymbolClass: Type = RootClass.newClass("org.apache.avro.generic.GenericEnumSymbol")
  val EnumClass: Type = RootClass.newClass("Enum")
  val Inl: Type = RootClass.newClass("Inl")
  val Inr: Type = RootClass.newClass("Inr")
}

class AvscGenScala(val settings: GeneratorSettings, val schema: Schema, val schemaName: String) {
  val schemaObjectSchemaValName = "schema"

  case class GenRECORD(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends TopLevelGen {

    val objectSchemaValName = "schema"
    val fieldParamName = "field"
    val fieldsObjectName = "fields"
    val valueParamName = "value"
    val typeVarName = "fullType"
    val innerTypeVarName = "typeInOption"

    override val unitInfo: UnitInfo = UnitInfo(Option(schema.getNamespace), schema.getName)

    override def fileContent: Tree = {
      val fieldsGens: Seq[(Schema.Field, Gen)] = schema.getFields.asScala.zip(pendingGens).toList

      val file = BLOCK(fieldsGens.foldLeft(Set[String]()) { case (s, (field, gen)) => s ++ gen.imports }.toList.map(s => IMPORT(s)) ++ List(
        CASECLASSDEF(symbol) withParams fieldsGens.map[ValDef] { case (field, gen) => VAR(field.name(), gen.rootClass) } withParents SpecificRecordBaseClass := BLOCK(

          DEFTHIS withParams (List.empty) := THIS APPLY fieldsGens.zipWithIndex.map[Tree] { case ((field, gen), i) =>
            (REF(field.name()) := gen.defaultValue) withComment (i match {
              case 0 => " "
              case i => (i - 1).toString
            })
          },
          DEF("getSchema", SchemaClass) withFlags Flags.OVERRIDE := symbol DOT objectSchemaValName,
          DEF("get", AnyRefClass) withParams VAL(fieldParamName, IntClass) withFlags Flags.OVERRIDE :=
            PAREN(REF(fieldParamName) withAnnots ANNOT(SwitchClass) MATCH (
              fieldsGens.zipWithIndex.map { case ((field, gen), i) =>
                CASE(LIT(i)) ==> gen.getTree(this, field, i)
              } :+ CaseInvalidIndex)) AS AnyRefClass,
          DEF("getRaw", AnyRefClass) withParams VAL(fieldParamName, IntClass) :=
            PAREN(REF(fieldParamName) withAnnots ANNOT(SwitchClass) MATCH (
              fieldsGens.zipWithIndex.map { case ((field, gen), i) =>
                CASE(LIT(i)) ==> REF(field.name())
              } :+ CaseInvalidIndex)) AS AnyRefClass,
          DEF("put", UnitClass) withParams(VAL(fieldParamName, IntClass), VAL(valueParamName, AnyClass)) withFlags Flags.OVERRIDE :=
            (REF(fieldParamName) withAnnots ANNOT(SwitchClass)) MATCH (
              fieldsGens.zipWithIndex.map { case ((field, gen), i) =>
                CASE(LIT(i)) ==> gen.putTree(this, field, i, valueParamName)
              } :+ CaseInvalidIndex),
          DEF("putRaw", UnitClass) withParams(VAL(fieldParamName, IntClass), VAL(valueParamName, AnyClass)) :=
            (REF(fieldParamName) withAnnots ANNOT(SwitchClass)) MATCH (
              fieldsGens.zipWithIndex.map { case ((field, gen), i) =>
                CASE(LIT(i)) ==> BLOCK(REF(field.name()) := REF(valueParamName) AS gen.rootClass)
              } :+ CaseInvalidIndex),
        ),

        OBJECTDEF(symbol) := BLOCK(
          VAL(objectSchemaValName, SchemaClass) := schemaTreeBasedOnParent,
          OBJECTDEF(fieldsObjectName) := BLOCK(fieldsGens.map { case (field, gen) =>
            OBJECTDEF(field.name()) := BLOCK(List[Option[Tree]](
              Some(TYPEVAR(typeVarName) := gen.rootClass),
              gen.optionType.map(TYPEVAR(innerTypeVarName) := _),
            ).flatten)
          })
        )
      ))

      schema.getNamespace match {
        case null => file
        case namespace => file inPackage namespace
      }
    }

    override def pendingGens: List[Gen] = schema.getFields.asScala.zipWithIndex.map { case (field, i) => gen(field.schema(), schemaTree DOT "getFields" DOT "get" APPLY LIT(i) DOT "schema" APPLY Nil) }.toList


    val rootClass: Type = RootClass.newClass(schema.getNamespace match {
      case null => schema.getName
      case someNamespace: String => s"$someNamespace.${schema.getName}"
    })

    val symbol: Symbol = RootClass.newClass(schema.getName)

    private val CaseInvalidIndex = CASE(REF(fieldParamName)) ==> THROW(NEW(REF("IllegalArgumentException") APPLY (INTERP("s", LIT(s"invalid field index for $rootClass (RECORD of ${schema.getFields.size()} fields): "), REF(fieldParamName)))))

    override def defaultValue: Tree = NEW(rootClass APPLY List())

    val schemaTree: Tree = rootClass DOT objectSchemaValName
  }

  case class GenENUM(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends TopLevelGen {

    val objectSchemaValName = "schema"
    val symbol = REF(schema.getName)

    override val unitInfo: UnitInfo = UnitInfo(Option(schema.getNamespace), schema.getName)

    override def fileContent: Tree = {
      val tree: Tree = BLOCK(
        CLASSDEF(schema.getName) withFlags(Flags.SEALED, Flags.ABSTRACT) withParams(PARAM("name", StringClass), PARAM("ordinal", IntClass)) withCtorFlags Flags.PRIVATE withParents(
          EnumClass APPLYTYPE schema.getName APPLY(REF("name"), REF("ordinal")),
          GenericEnumSymbolClass APPLYTYPE schema.getName
        ) := BLOCK(
          DEF("getSchema", SchemaClass) withFlags Flags.OVERRIDE := symbol DOT objectSchemaValName,
        ),
        OBJECTDEF(schema.getName) := BLOCK(
          (VAL(objectSchemaValName, SchemaClass) := schemaTreeBasedOnParent) ::
            schema.getEnumSymbols.asScala.zipWithIndex.map[Tree] { case (sym, i) => CASEOBJECTDEF(sym) withParents (REF(schema.getName) APPLY(LIT(sym), LIT(i))) }.toList
        )
      )
      Option(schema.getNamespace).fold(tree)(tree inPackage _)
    }

    override def pendingGens: List[Gen] = Nil

    val rootClass: Type = RootClass.newClass(schema.getNamespace match {
      case null => schema.getName
      case someNamespace: String => s"$someNamespace.${schema.getName}"
    })

    override def defaultValue: Tree = rootClass DOT schema.getEnumSymbols.get(0)
  }

  case class GenFIXED(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenComplex {

    val fixedSize = schema.getFixedSize

    override def rootClass: Type = TYPE_ARRAY(ByteClass)

    override def defaultValue: Tree = NEW(rootClass) APPLY (LIT(fixedSize))
  }


  case class GenARRAY(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenComplex {

    val valueGen = gen(schema.getElementType, schemaTreeBasedOnParent DOT "getElementType")

    override def rootClass: Type = TYPE_ARRAY(valueGen.rootClass)

    override def defaultValue: Tree = ARRAY()

    override def apply(): Generation = Generation(Nil, List(valueGen))
  }

  case class GenMAP(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenComplex {

    val valueGen = gen(schema.getValueType, schemaTreeBasedOnParent DOT "getValueType")

    override def rootClass: Type = TYPE_MAP(StringClass, valueGen.rootClass)

    override def defaultValue: Tree = MAKE_MAP()

    override def apply(): Generation = Generation(Nil, List(valueGen))
  }

  case class GenUNION(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenComplex {


    val nonNullTypes: List[Schema] = schema.getTypes.asScala.filter(_.getType != Schema.Type.NULL).toList

    val includesNull: Boolean = nonNullTypes.length < schema.getTypes.size()

    val gens: List[Gen] = schema.getTypes.asScala.zipWithIndex.collect { case (schema, i) if schema.getType != Schema.Type.NULL => gen(schema, schemaTreeBasedOnParent DOT "getTypes" DOT "get" APPLY (LIT(i))) }.toList


    override def imports: Set[String] = if ((includesNull, nonNullTypes.length) == (true, 1)) Set()
    else List(":+:", "CNil", "Coproduct", "Inl", "Inr").map(c => s"shapeless.$c").toSet

    val insideOptionClass: Type =
      if ((includesNull, nonNullTypes.length) == (true, 1)) gens.head.rootClass
      else RootClass.newClass(s"${gens.map(_.rootClass).mkString(" :+: ")} :+: CNil")

    override def rootClass: Type = if (includesNull) TYPE_OPTION(insideOptionClass) else insideOptionClass


    override def optionType: Option[Type] = if (includesNull) Some(insideOptionClass) else None


    override def apply(): Generation = Generation(List.empty, gens)

    override def defaultValue: Tree = if (includesNull) NONE else REF("Coproduct") APPLY (gens.head.defaultValue)


    override def getTree(parent: Gen, field: Schema.Field, i: Int): Tree = {

      val ref: Tree = REF(field.name())

      val tree: Tree = nonNullTypes match {
        case _ :: Nil if includesNull => ref
        case _ => ref MATCH gens.foldRight[List[CaseDef]](List(CASE(ID(field.name()) withType "CNil") ==> (ref DOT "impossible" AS AnyRefClass))) { case (gen, caseDefs) =>
          List[CaseDef](
            CASE(Inl UNAPPLY (ID(field.name()) withType gen.rootClass)) ==> (ref AS AnyRefClass),
            CASE(Inr UNAPPLY ID(field.name())) ==> (ref MATCH caseDefs)
          )
        }
      }

      if (includesNull)
        ref DOT "fold" APPLYTYPE AnyRefClass APPLY NULL APPLY (LAMBDA(PARAM(field.name()) withType insideOptionClass) ==> (PAREN(tree) AS AnyRefClass))
      else
        tree
    }

    override def putTree(parent: Gen, field: Schema.Field, i: Int, valueParamName: String): Tree = super.putTree(parent, field, i, valueParamName)


  }

  case class GenBYTES(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = TYPE_ARRAY(ByteClass)

    override def defaultValue: Tree = ARRAY()
  }

  case class GenSTRING(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenSTRING.rootClass

    override def defaultValue: Tree = LIT("")
  }

  case class GenINT(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenINT.rootClass

    override def defaultValue: Tree = LIT(0)
  }

  case class GenLONG(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenLONG.rootClass

    override def defaultValue: Tree = LIT(0L)
  }

  case class GenFLOAT(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenFLOAT.rootClass

    override def defaultValue: Tree = LIT(0.0f)
  }

  case class GenDOUBLE(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenDOUBLE.rootClass

    override def defaultValue: Tree = LIT(0.0)
  }

  case class GenBOOLEAN(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenBOOLEAN.rootClass

    override def defaultValue: Tree = LIT(false)
  }

  case class GenNULL(schema: Schema, override val schemaTreeBasedOnParent: Tree) extends GenPrimitive {
    override def rootClass: Type = GenNULL.rootClass

    override def defaultValue: Tree = NULL

  }


  trait AvroType {
    def apply(schema: Schema, parent: Tree): Gen
  }

  trait AvroTypePrimitive extends AvroType

  trait AvroTypeComplext extends AvroType

  object GenRECORD extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenRECORD(schema, parent)
  }

  object GenENUM extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenENUM(schema, parent)
  }

  object GenARRAY extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenARRAY(schema, parent)


  }

  object GenMAP extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenMAP(schema, parent)
  }

  object GenUNION extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenUNION(schema, parent)
  }

  object GenFIXED extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenFIXED(schema, parent)
  }

  object GenBYTES extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenBYTES(schema, parent)
  }

  object GenSTRING extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenSTRING(schema, parent)

    val rootClass: Type = StringClass
  }

  object GenINT extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenINT(schema, parent)

    val rootClass: Type = IntClass
  }

  object GenLONG extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenLONG(schema, parent)

    val rootClass: Type = LongClass
  }

  object GenFLOAT extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenFLOAT(schema, parent)

    val rootClass: Type = FloatClass
  }

  object GenDOUBLE extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenDOUBLE(schema, parent)

    val rootClass: Type = DoubleClass
  }

  object GenBOOLEAN extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenBOOLEAN(schema, parent)

    val rootClass: Type = BooleanClass
  }

  object GenNULL extends AvroType {
    def apply(schema: Schema, parent: Tree) = new GenNULL(schema, parent)

    val rootClass: Type = NullClass


  }

  def gen(schema: Schema, schemaTree: Tree) =

    (schema.getType match {
      case Schema.Type.RECORD => GenRECORD

      case Schema.Type.ENUM => GenENUM
      case Schema.Type.ARRAY => GenARRAY
      case Schema.Type.MAP => GenMAP
      case Schema.Type.UNION => GenUNION

      case Schema.Type.FIXED => GenFIXED

      case Schema.Type.BYTES => GenBYTES

      case Schema.Type.STRING => GenSTRING
      case Schema.Type.INT => GenINT
      case Schema.Type.LONG => GenLONG
      case Schema.Type.FLOAT => GenFLOAT
      case Schema.Type.DOUBLE => GenDOUBLE
      case Schema.Type.BOOLEAN => GenBOOLEAN

      case Schema.Type.NULL => GenNULL
    })(schema, schemaTree)

  val schemaUnitInfo: UnitInfo = schemaName.split('.').toList.reverse match {
    case name :: packageParts => UnitInfo(Some(packageParts).filter(_.nonEmpty).map(_.reverse.mkString(".")), name)
  }

  val rootClass: Type = RootClass.newClass(schemaUnitInfo.full)

  lazy val treeForSchema: Tree = BLOCK(List[Option[List[Tree]]](
    Some(List[Tree](
      OBJECTDEF(schemaUnitInfo.className) := BLOCK(
        LAZYVAL(schemaObjectSchemaValName, SchemaClass) := NEW(SchemaParserClass) APPLY Nil DOT "parse" APPLY (LIST(schema.toString(true).trim().split("\r?\n").map(line => LIT(line) withComment " ")) DOT "mkString" APPLY LIT(""))
      )))
  ).flatten.flatMap[Tree](identity[List[Tree]]))

  lazy val forSchema: Generation = Generation(List(GeneratedUnit(schemaUnitInfo, schemaUnitInfo.packageName.fold(treeForSchema)(treeForSchema inPackage _))), List(gen(schema, rootClass DOT schemaObjectSchemaValName)))

  lazy val files: List[GeneratedUnit] = {

    LazyList.from(0).scanLeft(forSchema) { case (Generation(generatedUnits, gen :: pendingGen), i) =>

      val willGenerate = gen.generatedUnitInfo.fold(true)(toGen => !generatedUnits.exists(generated => {
        generated.generatedElement == toGen
      }))

      if (willGenerate) {
        val Generation(nextGeneratedUnits, newPendings) = gen()
        Generation(generatedUnits ++ nextGeneratedUnits, pendingGen ++ newPendings)
      } else {
        Generation(generatedUnits, pendingGen)
      }


    }.find(_.pendingGen.isEmpty).fold(List.empty[GeneratedUnit])(_.generatedUnits)

  }

  def toFiles(dir: Path, files: List[GeneratedUnit]) = {
    files.foreach { case GeneratedUnit(UnitInfo(packageName, className), fileContent) =>

      val path = packageName.fold(dir)(_.split('.').foldLeft(dir) { case (dir, packagePart) =>
        val path = dir.resolve(packagePart)
        Files.createDirectories(path)
        path
      })

      Files.write(path.resolve(s"$className.scala"), treeToString(fileContent).getBytes(StandardCharsets.UTF_8))
    }
  }
}

