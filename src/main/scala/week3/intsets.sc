import week3.{Empty, NonEmpty}

object intsets {
  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 4
  val t3 = new NonEmpty(1, new Empty, new Empty)
  t3 union t2
}