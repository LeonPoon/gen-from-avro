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

import org.apache.avro.Schema
import treehugger.forest._
import definitions._
import treehugger.forest
import treehuggerDSL._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IterableHasAsScala


case class UnitInfo(
                     packageName: Option[String],
                     className: String
                   )

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


class AvscGenScala(val settings: GeneratorSettings, val schema: Schema) {

  val SpecificRecordBaseClass = RootClass.newClass("org.apache.avro.specific.SpecificRecordBase")
  val SchemaClass = RootClass.newClass("org.apache.avro.Schema")
  val SwitchClass = RootClass.newClass("scala.annotation.switch")

  case class GenRECORD(schema: Schema) extends TopLevelGen {

    val objectSchemaValName = "schema"
    val fieldParamName = "field"
    val valueParamName = "value"

    override val unitInfo: UnitInfo = UnitInfo(Option(schema.getNamespace), schema.getName)

    override def fileContent: Tree = {
      val fieldsGens = schema.getFields.asScala.zip(pendingGens).toList

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
          VAL(objectSchemaValName, SchemaClass) := NEW(SchemaClass DOT "Parser") APPLY (Nil) DOT "parse" APPLY (LIST(
            schema.toString(true).split("\r?\n").zipWithIndex.map { case (s, i) => LIT(s) withComments (" ") }) DOT "mkString" APPLY (LIT(""))
            )
        )
      ))

      schema.getNamespace match {
        case null => file
        case namespace => file inPackage namespace
      }
    }

    override def pendingGens: List[Gen] = schema.getFields.asScala.map(field => field.schema()).map(gen).toList

    val symbol: Symbol = RootClass.newClass(schema.getName)

    val rootClass: Type = RootClass.newClass(schema.getNamespace match {
      case null => schema.getName
      case someNamespace: String => s"$someNamespace.${schema.getName}"
    })

    private val CaseInvalidIndex = CASE(REF(fieldParamName)) ==> THROW(NEW(REF("IllegalArgumentException") APPLY (INTERP("s", LIT(s"invalid field index for $rootClass (RECORD of ${schema.getFields.size()} fields): "), REF(fieldParamName)))))

    override def defaultValue: Tree = NEW(rootClass APPLY List())
  }

  case class GenENUM(schema: Schema) extends TopLevelGen {
    override val unitInfo: UnitInfo = ???

    override def fileContent: Tree = ???

    override def pendingGens: List[Gen] = ???

    override def rootClass: Type = ???

    override def defaultValue: Tree = ???
  }

  case class GenFIXED(schema: Schema) extends TopLevelGen {
    override val unitInfo: UnitInfo = ???

    override def fileContent: Tree = ???

    override def pendingGens: List[Gen] = ???

    override def rootClass: Type = ???

    override def defaultValue: Tree = ???
  }


  case class GenARRAY(schema: Schema) extends GenComplex {
    override def rootClass: Type = ???

    override def defaultValue: Tree = ???
  }

  case class GenMAP(schema: Schema) extends GenComplex {
    override def rootClass: Type = ???

    override def defaultValue: Tree = ???
  }

  case class GenUNION(schema: Schema) extends GenComplex {

    val nonNullTypes: List[Schema] = schema.getTypes.asScala.filter(_.getType != Schema.Type.NULL).toList

    val includesNull: Boolean = nonNullTypes.length < schema.getTypes.size()

    val gens: List[Gen] = nonNullTypes.map(gen)


    override def imports: Set[String] = if ((includesNull, nonNullTypes.length) == (true, 1)) Set()
    else List(":+:", "CNil", "Coproduct").map(c => s"shapeless.$c").toSet

    val insideOptionClass: Type =
      if ((includesNull, nonNullTypes.length) == (true, 1)) gens.head.rootClass
      else RootClass.newClass(s"${gens.map(_.rootClass).mkString(" :+: ")} :+: CNil")

    override def rootClass: Type = if (includesNull) TYPE_OPTION(insideOptionClass) else insideOptionClass


    override def apply(): Generation = Generation(List.empty, gens)

    override def defaultValue: Tree = if (includesNull) NONE else REF("Coproduct") APPLY (gens.head.defaultValue)
  }

  case class GenBYTES(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = TYPE_ARRAY(ByteClass)

    override def defaultValue: Tree = ARRAY()
  }

  case class GenSTRING(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenSTRING.rootClass

    override def defaultValue: Tree = LIT("")
  }

  case class GenINT(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenINT.rootClass

    override def defaultValue: Tree = LIT(0)
  }

  case class GenLONG(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenLONG.rootClass

    override def defaultValue: Tree = LIT(0L)
  }

  case class GenFLOAT(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenFLOAT.rootClass

    override def defaultValue: Tree = LIT(0.0f)
  }

  case class GenDOUBLE(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenDOUBLE.rootClass

    override def defaultValue: Tree = LIT(0.0)
  }

  case class GenBOOLEAN(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenBOOLEAN.rootClass

    override def defaultValue: Tree = LIT(false)
  }

  case class GenNULL(schema: Schema) extends GenPrimitive {
    override def rootClass: Type = GenNULL.rootClass

    override def defaultValue: Tree = UNIT
  }


  trait AvroType {
    def apply(schema: Schema): Gen
  }

  trait AvroTypePrimitive extends AvroType

  trait AvroTypeComplext extends AvroType

  object GenRECORD extends AvroType {
    def apply(schema: Schema) = new GenRECORD(schema)
  }

  object GenENUM extends AvroType {
    def apply(schema: Schema) = new GenENUM(schema)
  }

  object GenARRAY extends AvroType {
    def apply(schema: Schema) = new GenARRAY(schema)


  }

  object GenMAP extends AvroType {
    def apply(schema: Schema) = new GenMAP(schema)
  }

  object GenUNION extends AvroType {
    def apply(schema: Schema) = new GenUNION(schema)
  }

  object GenFIXED extends AvroType {
    def apply(schema: Schema) = new GenFIXED(schema)
  }

  object GenBYTES extends AvroType {
    def apply(schema: Schema) = new GenBYTES(schema)
  }

  object GenSTRING extends AvroType {
    def apply(schema: Schema) = new GenSTRING(schema)

    val rootClass: Type = StringClass
  }

  object GenINT extends AvroType {
    def apply(schema: Schema) = new GenINT(schema)

    val rootClass: Type = IntClass
  }

  object GenLONG extends AvroType {
    def apply(schema: Schema) = new GenLONG(schema)

    val rootClass: Type = LongClass
  }

  object GenFLOAT extends AvroType {
    def apply(schema: Schema) = new GenFLOAT(schema)

    val rootClass: Type = FloatClass
  }

  object GenDOUBLE extends AvroType {
    def apply(schema: Schema) = new GenDOUBLE(schema)

    val rootClass: Type = DoubleClass
  }

  object GenBOOLEAN extends AvroType {
    def apply(schema: Schema) = new GenBOOLEAN(schema)

    val rootClass: Type = BooleanClass
  }

  object GenNULL extends AvroType {
    def apply(schema: Schema) = new GenNULL(schema)

    val rootClass: Type = UnitClass


  }


  def gen(schema: Schema) =

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
    })(schema)


  lazy val files: List[GeneratedUnit] = {

    LazyList.from(0).scanLeft(gen(schema)()) { case (Generation(generatedUnits, gen :: pendingGen), i) =>


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

