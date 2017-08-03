package chapter13

import chapter07.Par
import chapter07.Par.Par

import scala.io.StdIn

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}
case object ReadLine extends Console[Option[String]] {
  def toPar = Par.lazyUnit(run)
  def toThunk = () => run
  def run: Option[String] =
    try Some(StdIn.readLine())
    catch { case e: Exception => None }
}
case class PrintLine(line: String) extends Console[Unit] {
  def toPar = Par.lazyUnit(println(line))
  def toThunk = () => println(line)
}
