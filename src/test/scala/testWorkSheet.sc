import java.nio.ByteBuffer

import com.gensler.scalavro.types.AvroType
import com.zenaptix.test.Cqsf602w
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil
import scodec.codecs.implicits._

val bte = 123.toBinaryString
val bitVec = BitVector(0x7B) //123
val codec = uint2 ~ uint4 ~ uint8
val codec1 = uint8
val codec3 = uint16
val result1 :Attempt[DecodeResult[Int]] = Codec.decode(bitVec)(codec1)
println(result1.require.value)
//val result2: Attempt[DecodeResult[(Int ~ Int ~ Int)]] = Codec.decode(y)(codec)
val codec2 = codec3.asInstanceOf[Codec[Any]].sizeBound.lowerBound
codec2.toString

val testInt:Int = 512
testInt.toHexString
val buf = ByteBuffer.allocate(8)
val byteArr = buf.putDouble(testInt).array()
buf.flip()
buf.getDouble


val buf2 = ByteBuffer.allocate(64)
buf2.putInt(1)
buf2.putInt(5)
buf2.putInt(0)
buf2.putInt(0)
buf2.putInt(12)
buf2.array

5.toByte.toString

BitVector(0x0F).slice(4,9).toBin

val col = 1 :: 2 :: 3:: 4 :: Nil
col.length
val newCol = col.dropRight(1) ::: 6 :: Nil
val Str = "helloworld"
s"${Str.last}${Str.head}${Str.tail.dropRight(1)}"
val str1 = "hello"
str1.map( char =>
char :: "A" :: Nil
)

List(1,2,3,4).foldLeft(0)((acc,next) => acc + next)

val theBestInt:Int = 100
theBestInt.toDouble

val padded = List(0, 0, 0, 0, 3, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0)
padded.indices
padded.length

val intSeqType = AvroType[Cqsf602w]



