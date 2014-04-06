package object utils {

  implicit class TupleOps[A, B](self: Tuple2[A, B]) {
    def mapLeft[R](f: A => R): Tuple2[R, B] = (f(self._1), self._2)
    def mapRight[R](f: B => R): Tuple2[A, R] = (self._1, f(self._2))
  }

}