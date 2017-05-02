import com.zenaptix.dsl._

val dec =  new Decimal(3,2,Some(Left),Some(Right),Some(3),Some(ASCII()))
dec match {
  case a:CobolType => a.enc match {
    case e:Option[ASCII] => println("ASCII")
  }
}
