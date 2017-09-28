
import com.sksamuel.avro4s.AvroSchema
import com.zenaptix.dsl._
import scala.collection.immutable.Seq
import scala.io.BufferedSource
import scala.meta._

@CopyBookMacro
  case class Schemas(source: BufferedSource, lines: String, forest: Seq[Group], roots: Seq[CBTree], namespace: String = "com.zenaptix.dsl") {
    def createSchemas = Files.createCaseClasses(forest, namespace) //todo: create case classes should happen as a macro at compile time to make class availible at runtime
    def schema = AvroSchema[String] //todo: inject type from macro
  }

