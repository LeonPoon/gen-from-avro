package com.codepowered.avro_based.avsc_gen_scala.test

import com.codepowered.avro_based.avsc_gen_scala.AvscGenScala
import org.apache.avro.Schema

import java.nio.file.Paths

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


  val s1 =
    """


    """

  val s2 =
    """

{
"type": "record",
"namespace": "x.ns1",
"name": "a",
"fields": [F1,{
"name":"f1",
"type":{
    "type": "record",
"namespace": "x.ns2",
    "name": "b",
    "fields": [
{
    "name":"b1",
    "type":"string"
    },
{
    "name":"b2",
    "type":"x.ns1.a"
    }
    ]
    }
}
]
}
    """.replace("F1", primitives.flatMap(p => List(s"""{"name":"${p}Field","type":"$p"}""") ++
      logicalTypes.getOrElse(p, Nil).map(log => s"""{"name":"${p}Field${log.replace('-', '_')}","type":"$p","logicalType":"$log"}""")).mkString(","))


  val sch = new Schema.Parser().parse(
    s2)

  val path = Paths.get(".").toAbsolutePath.resolve("target")
  println(path)

  val gen = new AvscGenScala(null, sch)
  gen.toFiles(path, gen.files)

}
