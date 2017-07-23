package chapter04

import Option._

object CarInsuranceService {

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String,
                              numberOfSpeedingTickets: String): Option[Double] = {

    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)

  }

}