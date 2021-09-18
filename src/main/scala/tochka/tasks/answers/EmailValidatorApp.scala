package tochka.tasks.answers

import emailvalidator4j.{EmailValidator => EmailValidator4J}

/**
  * Task 3
  * Написать программу, которая принимает строку в консоли и говорит это валидный email или нет
  */
object EmailValidatorApp extends App {

  println("Please write an email to check it:")
  val str = scala.io.StdIn.readLine()
  val res = EmailValidatorImpl.validate(str)
  println(if (res) s"[$str] is valid." else s"[$str] is invalid")
}

trait EmailValidator {
  def validate(email: String): Boolean
}

object EmailValidatorImpl extends EmailValidator {
  def validate(email: String): Boolean =
    (new EmailValidator4J).isValid(email)
}
