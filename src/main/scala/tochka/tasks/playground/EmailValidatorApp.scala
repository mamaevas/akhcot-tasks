package tochka.tasks.playground

import java.util.regex.Matcher

object EmailValidatorApp extends App {

  trait EmailValidator {
    def validate(emailStr: String): Boolean
  }

  case object RFC6530Validator extends EmailValidator {
    def validate(emailStr: String): Boolean = {
      // see https://stackoverflow.com/a/56612502
      val RFC6530Matcher: CharSequence => Matcher =
        """(?im)^(?=.{1,64}@)(?:("[^"\\]*(?:\\.[^"\\]*)*"@)|((?:[^\W_](?:\.(?!\.)|
          |[-!#\$%&'\*\+\/=\?\^`\{\}\|~\w])*)?[^\W_]@))(?=.{1,255}$)(?:(\[(?:\d{1,3}\.)
          |{3}\d{1,3}\])|((?:(?=.{1,63}\.)[^\W_][-\w]*[^\W_]*\.)+[^\W_](?:[^\W_]|
          |-){0,22}[^\W_])|((?=.{1,63}$)[^\W_][-\w]*))$""".stripMargin
          .replaceAll(System.lineSeparator(), "")
          .r
          .pattern
          .matcher(_)
      RFC6530Matcher(Option(emailStr).getOrElse("")).matches()
    }
  }

  case object RFC5322Validator extends EmailValidator {
    def validate(emailStr: String): Boolean = {
      // aka RFC 5322 Official Standard, see https://stackoverflow.com/a/201378
      val RFC5322Matcher: CharSequence => Matcher =
        """(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|
          |"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|
          |\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+
          |[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}
          |(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:
          |(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])""".stripMargin
          .replaceAll(System.lineSeparator(), "")
          .r
          .pattern
          .matcher(_)

      RFC5322Matcher(Option(emailStr).getOrElse("")).matches()
    }
  }

  case object EmailValidatorImpl extends EmailValidator {
    def validate(emailStr: String): Boolean = {
      import emailvalidator4j.{EmailValidator => EmailValidator4J}
      (new EmailValidator4J).isValid(emailStr)
    }
  }

  val validEmailList = List(
    // handled by RFC 5322 based servers
    "simple@example.com",
    "very.common@example.com",
    "disposable.style.email.with+symbol@example.com",
    "other.email-with-hyphen@example.com",
    "fully-qualified-domain@example.com",
    "user.name+tag+sorting@example.com", // (may go to user.name@example.com inbox depending on mail server)
    "x@example.com", // (one-letter local-part)
    "example-indeed@strange-example.com",
    "test/test@test.com", // (slashes are a printable character, and allowed)
    "admin@mailserver1", // (local domain name with no TLD, although ICANN highly discourages dotless email addresses[10])
    "example@s.example", // (see the List of Internet top-level domains)
    "\" \"@example.org", // (space between the quotes)
    "\"john..doe\"@example.org", // (quoted double dot)
    "mailhost!username@example.org", // (bangified host route used for uucp mailers)
    "user%example.com@example.org", // (% escaped mail route to user@example.com via example.org)
    "user-@example.org", // (local part ending with non-alphanumeric character from the list of allowed printable characters)

    // The example addresses below would not be handled by RFC 5322 based servers, but are permitted by RFC 6530
    "Pelé@example.com", // Latin alphabet with diacritics
    "δοκιμή@παράδειγμα.δοκιμή", // Greek alphabet
    "我買@屋企.香港", // Traditional Chinese characters
    "二ノ宮@黒川.日本", // Japanese characters
    "медведь@с-балалайкой.рф", // Cyrillic characters
    "संपर्क@डाटामेल.भारत" // Devanagari characters
  )

  val invalidEmailList = List(
    "Abc.example.com", // (no @ character)
    "A@b@c@example.com", // (only one @ is allowed outside quotation marks)
    "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com", // (none of the special characters in this local-part are allowed outside quotation marks)
    "just\"not\"right@example.com", // (quoted strings must be dot separated or the only element making up the local-part)
    "this is\"not\\allowed@example.com", // (spaces, quotes, and backslashes may only exist when within quoted strings and preceded by a backslash)
    "this\\ still\\\"not\\\\allowed@example.com", // (even if escaped (preceded by a backslash), spaces, quotes, and backslashes must still be contained by quotes)
    "1234567890123456789012345678901234567890123456789012345678901234+x@example.com", // (local-part is longer than 64 characters)
    "i_like_underscore@but_its_not_allowed_in_this_part.example.com", // (Underscore is not allowed in domain part)
    "QA[icon]CHOCOLATE[icon]@test.com" // (icon characters)
  )

  def test(validator: EmailValidator) = {
    println(s"Test ${validator.getClass.getSimpleName}:")
    println("------------------")
    val validRes = validEmailList.filter(!validator.validate(_))
    println(s"Failed [${validRes.length}] emails from valid email list: ${validRes.mkString(", ")}")
    println()
    val invalidRes = invalidEmailList.filter(validator.validate)
    println(s"Failed [${invalidRes.length}] emails from invalid email list: ${invalidRes.mkString(", ")}")
    println("------------------")
    println()
    (validRes.length + invalidRes.length, validator.getClass.getSimpleName)
  }

  def testAll = {
    val res = List(
      test(RFC6530Validator),
      test(RFC5322Validator),
      test(EmailValidatorImpl)
    ).sortBy {
      case (failSize, _) => failSize
    }
    println(s"Results (the best result is first): ${res.mkString(", ")}")
  }

  testAll
}
