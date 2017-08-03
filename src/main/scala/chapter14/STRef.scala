package chapter14

sealed trait STRef[S, A] {

  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    override def run(s: S) = {
      cell = a
      ((), s)
    }
  }

}

object STRef {

  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })

  def main(args: Array[String]): Unit = {

    for {
      r1 <- STRef[Nothing, Int](1)
      r2 <- STRef[Nothing, Int](1)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)

    STRef[Nothing, Int](1)
      .flatMap(r1 => STRef[Nothing, Int](1)
        .flatMap(r2 => r1.read
          .flatMap(x => r2.read
            .flatMap(y => r1.write(y + 1)
              .flatMap(_ => r2.write(x + 1)
                .flatMap(_ => r1.read
                  .flatMap(a => r2.read
                    .map(b => (a, b)))))))))


  }

}