package o1.sound.midi
import o1.util.nice.number.*

/** A companion for [[Pitch class `Pitch`]]. */
object Pitch:
  private[midi] val OctaveSemitones = "C D EF G A H"
  private[midi] val MinID           = 0
  private[midi] val MaxID           = 127

/** A `Pitch` is a frequency for a note to be played at.
  *
  * @param name        the name of the corresponding note, which provides the base frequency;
  *                    one of "cdefgah" or their upper-case equivalents
  * @param accidental  an adjustment to the base frequency
  * @param octave      the number of the octave relative to [[DefaultOctave]]; e.g., -2 means two octaves
  *                    lower than the default; passing in `None` has the same effect as passing in zero */
final case class Pitch(val name: Char, val accidental: Accidental, val octave: Option[Int]) extends OccursInChord:
  import Pitch.*

  private val numberWithinOctave = OctaveSemitones.indexOf(this.name.toUpper)

  private[midi] lazy val stringID =
    val octaveDiff = this.octave.map( _ - DefaultOctave ).getOrElse(0)
    val octaveMark = if octaveDiff < 0 then "<" * -octaveDiff else ">" * octaveDiff
    octaveMark + this.name + this.accidental

  private[midi] def midiID(defaultOctave: Int) =
    val octave = this.octave.getOrElse(defaultOctave)
    val octaveStartID = OctaveSemitones.length * octave
    val globalNumber = octaveStartID + this.numberWithinOctave + this.accidental.shift
    globalNumber atLeast MinID atMost MaxID

  private[midi] def whenInChord(chordLength: Int, chordIsStaccato: Boolean) = Note(this, chordLength, chordIsStaccato)

  /** Returns a string description of the pitch. */
  override def toString = this.stringID

end Pitch

/** An adjustment to the frequency of a [[Pitch]]: a `Flat`, a `Sharp`, or a `Natural`.
  * @param shift   the shift from the base frequency caused by the accidental (in semitones).
  *                A positive number means a higher frequency, a negative number a lower one.
  * @param symbol  a symbol describing the accidental: "#", "♭", or the empty string */
enum Accidental(val shift: Int, val symbol: String):
  /** Marks a flat note. Assigning this accidental to a [[Pitch]] makes it one semitone lower. */
  case Flat extends Accidental(-1, "♭")
  /** Marks a sharp note. Assigning this accidental to a [[Pitch]] makes it one semitone higher. */
  case Sharp extends Accidental(+1, "#")
  /** Marks an unchanged, natural note (neither [[Sharp]] nor [[Flat]]). */
  case Natural extends Accidental(0, "")
  /** a short string representing the accidental */
  override def toString = this.symbol
end Accidental

