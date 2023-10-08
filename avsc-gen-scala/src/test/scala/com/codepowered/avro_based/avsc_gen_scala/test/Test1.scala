package com.codepowered.avro_based.avsc_gen_scala.test

import com.codepowered.avro_based.avsc_gen_scala.AvscGenScala
import org.apache.avro.Schema
import treehugger.forest._
import definitions._
import treehuggerDSL._

import java.nio.file.Paths
import scala.io.Source

object Test1 extends App {


  val primitives = List(
    "null",
    "boolean",
    "int",
    "long",
    "float",
    "double",
    "bytes",
    "string")

  val logicalTypes = Map[String, List[String]]("int" -> List("date", "time-millis"), "long" -> List("time-micros",
    "timestamp-millis", "timestamp-micros", "local-timestamp-millis", "local-timestamp-micros"
  ), "byte" -> List("decimal"), "fixed" -> List("decimal", "duration"), "string" -> List("uuid"))

  val stxt = Source.fromInputStream(getClass.getResourceAsStream("/com/codepowered/avro_based/avsc_gen_scala/test/schema.avsc")).mkString

  val s2 =
    stxt.replace("F1", primitives.flatMap(p => List(s"""{"name":"${p}Field","type":"$p"}""") ++
        logicalTypes.getOrElse(p, Nil).map(log => s"""{"name":"${p}Field${log.replace('-', '_')}","type":"$p","logicalType":"$log"}""")).mkString(","))
      .replace("F2", s"""{"name":"unionField1","type":[${(primitives ++ List("x.ns1.a")).map(s => s"\"$s\"").mkString(",\n")}]}"""
      )



  val sch = new Schema.Parser().parse(
    s2)

  println(sch.toString(true))

  val path = Paths.get(".").toAbsolutePath.resolve("target")
  println(path)

  val gen = new AvscGenScala(null, sch, "x.MyAvro")
  gen.toFiles(path, gen.files)

}
