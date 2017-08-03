package chapter14

trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {

    override def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {

    override def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }

  }

}

object ST {

  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      override def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply.run(())._1

  def main(args: Array[String]): Unit = {

    val p = new RunnableST[(Int, Int)] {
      override def apply[S] =
        for {
          r1 <- STRef(1)
          r2 <- STRef(1)
          x <- r1.read
          y <- r2.read
          _ <- r1.write(y + 1)
          _ <- r2.write(x + 1)
          a <- r1.read
          b <- r2.read
        } yield (a, b)
    }

    val r = new RunnableST[(Int, Int)] {
      override def apply[S] =
        STRef[S, Int](1)
          .flatMap(r1 => STRef[S, Int](1)
            .flatMap(r2 => r1.read
              .flatMap(x => r2.read
                .flatMap(y => r1.write(y + 1)
                  .flatMap(_ => r2.write(x + 1)
                    .flatMap(_ => r1.read
                      .flatMap(a => r2.read
                        .map(b => (a, b)))))))))


    }

    println(runST(p))

    println(runST(r))

  }

}
