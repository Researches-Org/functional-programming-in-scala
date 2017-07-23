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

    //A common idiom is to do o.getOrElse(throw new Exception("FAIL")) to convert
    // the None case of an Option back to an exception. The general rule of thumb is
    // that we use exceptions only if no reasonable program would ever catch the exception;
    // if for some callers the exception might be a recoverable error, we use Option (or
    // Either) to give them flexibility.
    // As you can see, returning errors as ordinary values can be convenient and the use
    // of higher-order functions lets us achieve the same sort of consolidation of error handling
    // logic we would get from using exceptions. Note that we don’t have to check
    // for None at each stage of the computation—we can apply several transformations and
    // then check for and handle None when we’re ready. But we also get additional safety,
    // since Option[A] is a different type than A, and the compiler won’t let us forget to
    // explicitly defer or handle the possibility of None.
    val dept: String =
      lookupByName("Joe").
        map(_.department).
        filter(_ != "IT").
        getOrElse("Default Dept")

    println(s"dept: ${dept}")

  }

}
