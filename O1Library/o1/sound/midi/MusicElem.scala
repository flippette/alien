package o1.sound.midi

/** A `MusicElem` is an element describes an aspect of MIDI music: a note, a chord, a pause,
  * or a meta directive such as the change of an instrument. `MusicElem`s put together make a
  * [[Voice]]; voices put together compose a piece of [[Music]].
  *
  * Each `MusicElem` has a `length` that represents how long it takes to play it. The duration
  * of the shortest audible sound playable with `o1.sound.midi` is one; other durations are
  * linearly relative to that.
  *
  * @see the subtypes [[Note]], [[Chord]], [[Pause]], [[OctaveShift]], [[Instrument]] */
trait MusicElem derives CanEqual:
  /** the duration of the `MusicElem`: how long it takes to play it */
  def length: Int


/** Each instance of this class represents a single musical note at a set pitch.
  *
  * @param pitch       the pitch of the note
  * @param length      the duration of the note: how long it takes to play it
  * @param isStaccato  whether the note should be played *staccato* so that
  *                    sound is shorter than its duration */
final case class Note(val pitch: Pitch, val length: Int, isStaccato: Boolean) extends MusicElem:

  /** the duration that the note is audible. This equals the `length` of the note
    * except for *staccato* notes whose audible length is half their `length` */
  def audibleLength = if this.isStaccato then this.length / 2 else this.length

  /** the volume the note should be played at. The `Note` class selects between
    * [[HighVolume]] and [[MediumVolume]] on the basis of the letter case of
    * the note’s [[Pitch]]. */
  def volume = if this.isLoud then HighVolume else MediumVolume

  private lazy val isLoud = this.pitch.name.isUpper

  /** Returns a string description of the note. */
  override def toString =
    val staccato = if this.isStaccato then "s" else ""
    val loudness = if this.isLoud     then "!" else ""
    s"${pitch}_$length$staccato$loudness"

end Note


private[midi] trait OccursInChord:
  private[midi] def whenInChord(chordLength: Int, chordIsStaccato: Boolean): MusicElem


/** Each instance of this class represents a combination of simultaneous [[Note]]s
  * or other [[MusicElem]]s.
  * @param notes  the `MusicElem`s that play simultanously in this chord */
final case class Chord(val notes: Seq[MusicElem]) extends MusicElem:
  /** the duration of the chord: how long it takes to play its longest element */
  lazy val length = this.notes.map( _.length ).max

  /** Returns a string description of the note. */
  override def toString = s"(${notes.mkString(" ")})"
end Chord
object Chord:
  private[midi] def apply(chordBits: Seq[OccursInChord], length: Int, isStaccato: Boolean): Chord =
    new Chord(chordBits.map( _.whenInChord(length, isStaccato) ))
end Chord


/** This object is a `MusicElem` that represents a short pause in the music. */
object Pause extends MusicElem:
  /** The length of the pause is 2: twice the length of the shortest playable MIDI sound. */
  val length = 2
  /** Returns a string description of a pause. */
  override def toString = "..."
end Pause


/** Each instance of this class is a directive that instructs the subsequent notes to
  * play one octave higher or lower.
  * @param shift  the direction of the shift: +1 means one octave higher, -1 one lower */
final case class OctaveShift(val shift: Int) extends MusicElem, OccursInChord:
  /** an `OctaveShift` is a meta directive; it has no duration, so its length is zero */
  val length = 0

  private[midi] def whenInChord(chordLength: Int, chordIsStaccato: Boolean) = this

  /** Returns a string description of the octave shift. */
  override def toString = if shift > 0 then ">" else if shift < 0 then "<" else "<?>"

end OctaveShift

