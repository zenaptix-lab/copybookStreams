import scala.collection.immutable.Seq
import scala.meta._

q"def foo[A <: B](implicit e: A): A".structure

q"com.sksamuel.avro4s.AvroSchema[T]".structure

q"scala.Int".structure

