package com.codepowered.avro_based.avsc_gen_scala.test

import com.codepowered.avro_based.avsc_gen_scala.{AvscGenScala, UnitInfo}
import org.apache.avro.Schema
import treehugger.forest._
import definitions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import treehuggerDSL._

import java.nio.file.{Files, Path, Paths}

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

  it should s"generate for fixed schema" in {
    val schema = new Schema.Parser().parse(Schema.createFixed("someFixed", null, "gen.for_._fixed", 10).toString(true))
    val generated = new AvscGenScala(null, schema, s"gen.for_._fixed.MyAvroSchema").files
    AvscGenScala.toFiles(generateTo, generated)
    generated.head.generatedElement shouldBe UnitInfo(Some("gen.for_._fixed"), "MyAvroSchema")
    generated.tail.head.generatedElement shouldBe UnitInfo(Some("gen.for_._fixed"), "someFixed")
    generated.tail.tail shouldBe empty
  }

}
