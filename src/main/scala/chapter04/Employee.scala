package chapter04

case class Employee(name: String, department: String)

object Employee {

  var employees = Seq[Employee]()

  def lookupByName(name: String): Option[Employee] =
    employees.find(e => e.name == name) match {
      case scala.None =>  None
      case scala.Some(e) => Some(e)
    }

  def add(name: String, department: String) = {
    employees = employees :+ Employee(name, department)
  }
}

import Employee._

object EmployeeApp {

  def main(args: Array[String]): Unit = {

    add("Joe", "IT")
    add("Manoel", "IT")
    add("Darko", "IT")
    add("Tony", "HR")

    println(employees)

    val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
    println("Joe's department: " + joeDepartment)

    val alynneDepartment: Option[String] = lookupByName("Alynne").map(_.department)
    println("Alynne's department: " + alynneDepartment)

  }

}
