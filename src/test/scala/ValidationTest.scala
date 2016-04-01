import org.scalatest.Matchers
import java.time.Period
import java.time.LocalDate
import org.scalatest.WordSpec
import scalaz._
import scalaz.Scalaz._

// based on (and fixes) examples in https://www.innoq.com/en/blog/validate-your-domain-in-scala/
class ValidationTest extends WordSpec with Matchers {
  import Model._

  "validates" in {
    val opeth = Band("Opeth")
    val mikael = Musician(
      name = "Mikael Åkerfeldt",
      born = LocalDate.parse("1974-04-17"),
      instruments = List(Guitar, BassGuitar),
      currentBand = Option(opeth)
    )
    val badMikael = mikael.copy(born = LocalDate.now.minusYears(2), instruments = Nil)

    validate(mikael) shouldBe scalaz.Success(mikael)
    validate(badMikael) shouldBe scalaz.Failure(NonEmptyList("too young", "at least one instrument"))
  }
}

object Model {
  trait Classification
  case object StringInstrument extends Classification
  case object Keyboard extends Classification
  abstract class Instrument(val classification: Classification)
  case object BassGuitar extends Instrument(StringInstrument)
  case object Guitar extends Instrument(StringInstrument)
  case object Piano extends Instrument(Keyboard)
  case class Band(name: String)
  case class MemberOfBand(from: LocalDate, membership: Period)
  case class Musician(
    name: String,
    born: LocalDate,
    instruments: Seq[Instrument],
    currentBand: Option[Band] = None,
    formerBands: Seq[MemberOfBand] = Nil
  )

  def validate(musician: Musician): ValidationNel[String, Musician] = {
    def validName(name: String): ValidationNel[String, String] =
      name match {
        case "" ⇒ scalaz.Failure("name must not be empty").toValidationNel
        case _  ⇒ name.success
      }

    def validateAge(born: LocalDate): ValidationNel[String, LocalDate] =
      if (born.isAfter(LocalDate.now().minusYears(12)))
        scalaz.Failure("too young").toValidationNel
      else born.success

    def validInstrument(instruments: Seq[Instrument]): ValidationNel[String, Seq[Instrument]] =
      instruments match {
        case Nil ⇒ scalaz.Failure("at least one instrument").toValidationNel
        case _   ⇒ instruments.success
      }

    (validName(musician.name)
      |@| validateAge(musician.born)
      |@| validInstrument(musician.instruments))((a, b, c) ⇒ musician)
  }
}
