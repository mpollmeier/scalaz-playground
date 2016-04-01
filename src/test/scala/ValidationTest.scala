import org.scalatest.Matchers
import java.time.Period
import java.time.LocalDate
import org.scalatest.WordSpec
import scalaz._
import scalaz.Scalaz._

// based on (and fixes) use case in https://www.innoq.com/en/blog/validate-your-domain-in-scala/
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

    val badMikael = mikael.copy(born = LocalDate.now.minusYears(2))
      .copy(instruments = Nil)

    println(validate(mikael))
    println(validate(badMikael))
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

  type StringValidation[T] = Validation[String, T]
  // type ValidationNel[+E, +X] = Validation[NonEmptyList[E], X]

  def validate(musician: Musician): StringValidation[Musician] = {
    def validName(name: String): StringValidation[String] =
      name match {
        case "" ⇒ "name must not be empty".failure
        case _  ⇒ name.success
      }

    def validateAge(born: LocalDate): StringValidation[LocalDate] =
      if (born.isAfter(LocalDate.now().minusYears(12))) "too young".failure
      else born.success

    def validInstrument(instruments: Seq[Instrument]): StringValidation[Seq[Instrument]] =
      instruments match {
        case Nil ⇒ "at least one instrument".failure
        case _   ⇒ instruments.success
      }

    (validName(musician.name)
      |@| validateAge(musician.born)
      |@| validInstrument(musician.instruments))((a, b, c) ⇒ musician)
  }
}
