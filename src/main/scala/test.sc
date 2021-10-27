def odd: Int=>Option[Int] = a=> if(a%2==0) Some(a) else None
odd(1)
val v:Int  = Some(2).value
odd(3)

val i = 10