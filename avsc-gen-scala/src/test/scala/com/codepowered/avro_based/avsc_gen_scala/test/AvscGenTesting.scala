package com.codepowered.avro_based.avsc_gen_scala.test

import com.codepowered.avro_based.avsc_gen_scala.{AvscGenScala, UnitInfo}
import org.apache.avro.Schema
import treehugger.forest._
import definitions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import treehuggerDSL._

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.SeqHasAsJava

class AvscGenTesting extends AnyFlatSpec with should.Matchers {


  val primitives = List(
    "null",
    "boolean",
    "int",
    "long",
    "float",
    "double",
    "bytes",
    "string")

  val complexTypes = List(
    "record",
    "union",
    "enum",
    "fixed",
    "map",
    "array"
  )

  val logicalTypes = Map[String, List[String]](
    "int" -> List("date", "time-millis"),
    "long" -> List(
      "time-micros",
      "timestamp-millis",
      "timestamp-micros",
      "local-timestamp-millis",
      "local-timestamp-micros"
    ),
    "byte" -> List("decimal"),
    "fixed" -> List("decimal", "duration"),
    "string" -> List("uuid"))

  val generateTo: Path = Paths.get("avsc-gen-scala/target/testGenerated/AvscGenScala").toAbsolutePath
  Files.createDirectories(generateTo)
  Files.list(generateTo).forEach { p =>
    def del(p: Path): Unit = {
      if (Files.isDirectory(p)) Files.list(p).forEach(del)
      println(s"delete: $p")
      Files.delete(p)
    }

    del(p)
  }

  def re_schema(schema: Schema): Schema = new Schema.Parser().parse(schema.toString())

  behavior of "AvscGenScala"

  primitives.foreach { primitive =>
    it should s"generate nothing for $primitive schema" in {
      val schema = new Schema.Parser().parse(Schema.create(Schema.Type.valueOf(primitive.toUpperCase)).toString(true))
      val generated = new AvscGenScala(null, schema, s"gen.nothing.for_.primitive.MyAvroSchema_$primitive").files
      AvscGenScala.toFiles(generateTo, generated)
      generated.tail shouldBe empty
      generated.head.generatedElement shouldBe UnitInfo(Some("gen.nothing.for_.primitive"), s"MyAvroSchema_$primitive")
    }
  }

  it should "generate nothing for array schema" in {
    val schema = new Schema.Parser().parse(Schema.createArray(Schema.create(Schema.Type.INT)).toString(true))
    val generated = new AvscGenScala(null, schema, "gen.nothing.for_._array.MyAvroSchemaArray").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.tail shouldBe empty
    generated.head.generatedElement shouldBe UnitInfo(Some("gen.nothing.for_._array"), "MyAvroSchemaArray")
  }

  it should "generate nothing for map schema" in {
    val schema = new Schema.Parser().parse(Schema.createMap(Schema.create(Schema.Type.INT)).toString(true))
    val generated = new AvscGenScala(null, schema, "gen.nothing.for_._map.MyAvroSchemaMap").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.tail shouldBe empty
    generated.head.generatedElement shouldBe UnitInfo(Some("gen.nothing.for_._map"), "MyAvroSchemaMap")
  }

  def fixedSchema(name: String, space: Option[String], size: Int): Schema = new Schema.Parser().parse(Schema.createFixed(name, null, space.orNull, size).toString())

  it should "generate for fixed schema" in {
    val schema = fixedSchema("someFixed", Some("gen.for_._fixed"), 10)
    val generated = new AvscGenScala(null, schema, "gen.for_._fixed.MyAvroSchemaFixed10").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some("gen.for_._fixed"), "MyAvroSchemaFixed10")
    generated.tail.head.generatedElement shouldBe UnitInfo(Some("gen.for_._fixed"), "someFixed")
    generated.tail.tail shouldBe empty
  }

  def enumSchema(name: String, namespace: Option[String], values: List[String]): Schema = new Schema.Parser().parse(Schema.createEnum(name, null, namespace.orNull, values.asJava).toString())

  it should "generate enum" in {
    val schema = enumSchema("Enum1", Some("gen.for_._enum1"), List("Apple", "Orange"))
    val generated = new AvscGenScala(null, schema, "gen.for_._enum1.MyAvroSchema_enum1").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some("gen.for_._enum1"), "MyAvroSchema_enum1")
    generated.tail.head.generatedElement shouldBe UnitInfo(Some("gen.for_._enum1"), "Enum1")
    generated.tail.tail shouldBe empty
  }


  val nsForRecordsOfVariousUnions = "gen.for_.records.containing_various_unions"

  val schemaUnionEmpty = Schema.createRecord("UnionEmpty", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("emptyUnionField", new Schema.Parser().parse(Schema.createUnion().toString()))).asJava)
  val schemaUnionOfNull = Schema.createRecord("UnionOfNull", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionOfNullField", new Schema.Parser().parse(Schema.createUnion(Schema.create(Schema.Type.NULL)).toString()))).asJava)
  val schemaUnionOfOneNonNullType = Schema.createRecord("UnionOfOneNonNullType", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionOfOneNonNullTypeField", new Schema.Parser().parse(Schema.createUnion(schemaUnionOfNull).toString()))).asJava)
  val schemaUnionOfNullWithOneNonNullType = Schema.createRecord("UnionOfNullWithOneNonNullType", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionOfNullWithOneNonNullTypeField", new Schema.Parser().parse(Schema.createUnion(Schema.create(Schema.Type.NULL), schemaUnionOfOneNonNullType).toString()))).asJava)
  val schemaUnionOfTwoNonNullType = Schema.createRecord("UnionOfTwoNonNullType", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionOfTwoNonNullTypeField", new Schema.Parser().parse(Schema.createUnion(schemaUnionOfOneNonNullType, schemaUnionOfNullWithOneNonNullType).toString()))).asJava)
  val schemaUnionNullWithTwoNonNullType = Schema.createRecord("UnionNullWithTwoNonNullType", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionNullWithTwoNonNullTypeField", new Schema.Parser().parse(Schema.createUnion(Schema.create(Schema.Type.NULL), schemaUnionOfNullWithOneNonNullType, schemaUnionOfTwoNonNullType).toString()))).asJava)
  val schemaUnionOfMoreThanTwoNonNullType = Schema.createRecord("UnionOfMoreThanTwoNonNullType", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionOfMoreThanTwoNonNullTypeField", new Schema.Parser().parse(Schema.createUnion(schemaUnionOfNullWithOneNonNullType, schemaUnionOfTwoNonNullType, schemaUnionNullWithTwoNonNullType).toString()))).asJava)
  val schemaUnionNullWithMoreThanTwoNonNullType = Schema.createRecord("UnionNullWithMoreThanTwoNonNullType", null, nsForRecordsOfVariousUnions, false, List(new Schema.Field("unionNullWithMoreThanTwoNonNullTypeField", new Schema.Parser().parse(Schema.createUnion(Schema.create(Schema.Type.NULL), schemaUnionOfTwoNonNullType, schemaUnionNullWithTwoNonNullType, schemaUnionOfMoreThanTwoNonNullType).toString()))).asJava)


  it should "generate records of various union combinations" in {
    val schema = new Schema.Parser().parse(Schema.createUnion(
      schemaUnionEmpty,
      schemaUnionOfNull,
      schemaUnionOfOneNonNullType,
      schemaUnionOfNullWithOneNonNullType,
      schemaUnionOfTwoNonNullType,
      schemaUnionNullWithTwoNonNullType,
      schemaUnionOfMoreThanTwoNonNullType,
      schemaUnionNullWithMoreThanTwoNonNullType
    ).toString())
    val generated = new AvscGenScala(null, schema, s"$nsForRecordsOfVariousUnions.MyAvroSchemaRecordsVariousUnions").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some(nsForRecordsOfVariousUnions), "MyAvroSchemaRecordsVariousUnions")
    generated.tail shouldNot be(empty)
  }

  it should "generate for record of primitive typed fields" in {
    val ns = "gen.record.primitive_fields"
    val schema = new Schema.Parser().parse(Schema.createRecord("PrimitiveFieldsRecord", null, ns, false, (primitives.map[Schema.Field] { typeName =>
      new Schema.Field(s"${typeName}Field", Schema.create(Schema.Type.valueOf(typeName.toUpperCase())), null)
    } ++ List(
      new Schema.Field("enumField", enumSchema("MyEnum1", Some(ns), List("Apple", "Google"))),
      new Schema.Field("fixedField", fixedSchema("MyFix", Some(ns), 9))
    )).asJava).toString())
    val generated = new AvscGenScala(null, schema, s"$ns.MyAvroSchemaRecordOfFieldsOfPrimitives").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some(ns), "MyAvroSchemaRecordOfFieldsOfPrimitives")
    generated.tail shouldNot be(empty)
  }

  it should "generate for record of fields of arrays and maps" in {
    val ns = "gen.record.containers"
    val moreSchemas: List[(String, Schema)] = List(
      "enumField" -> enumSchema("MyEnum2", Some(ns), List("Windows", "linux")),
      "fixedField" -> fixedSchema("MyFix2", Some(ns), 9)
    )
    val schema = re_schema(Schema.createUnion(
      Schema.createRecord("RecordOfArrays", null, ns, false, (primitives.map[Schema.Field] { typeName =>
        new Schema.Field(s"${typeName}Field", Schema.createArray(Schema.create(Schema.Type.valueOf(typeName.toUpperCase()))))
      } ++ moreSchemas.map[Schema.Field] { case (n, s) => new Schema.Field(n, s) }).asJava),
      Schema.createRecord("RecordOfMaps", null, ns, false, (primitives.map[Schema.Field] { typeName =>
        new Schema.Field(s"${typeName}Field", Schema.createMap(Schema.create(Schema.Type.valueOf(typeName.toUpperCase()))))
      } ++ moreSchemas.map[Schema.Field] { case (n, s) => new Schema.Field(n, s) }).asJava)
    ))
    val generated = new AvscGenScala(null, schema, s"$ns.MyAvroSchemaRecordsOfMapsAndArrays").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some(ns), "MyAvroSchemaRecordsOfMapsAndArrays")
    generated.tail shouldNot be(empty)
  }

  it should "generate for record of fields of stacked arrays and maps" in {
    val ns = "gen.record.containers.stack_array_map"
    val schema = re_schema(Schema.createRecord("RecordStackOfMapsAndArrays", null, ns, false, List(
      new Schema.Field("fieldA", Schema.createUnion(Schema.createArray(Schema.createMap(Schema.createArray(Schema.createUnion(primitives.map(n => Schema.create(Schema.Type.valueOf(n.toUpperCase))).asJava)))))),
      new Schema.Field("fieldM", Schema.createUnion(Schema.createMap(Schema.createArray(Schema.createMap(Schema.createUnion(primitives.map(n => Schema.create(Schema.Type.valueOf(n.toUpperCase))).asJava))))))
    ).asJava))
    val generated = new AvscGenScala(null, schema, s"$ns.MyAvroSchemaRecordStackOfMapsAndArrays").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some(ns), "MyAvroSchemaRecordStackOfMapsAndArrays")
    generated.tail shouldNot be(empty)
  }
}
