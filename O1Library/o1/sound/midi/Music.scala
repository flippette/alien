package o1.sound.midi
import o1.util.nice.number.*

/** This companion object of [[Music class `Music`]] provides additional options for creating `Music` objects. */
object Music:

  /** Constructs a music object from the given string, which must follow the format specified in
    * the [[o1.sound.midi package overview]]. An `IllegalArgumentException` is thrown if it doesn’t.
    * (This is equivalent to parsing the string with [[parse]].)  */
  def fromString(musicString: String): Music = parse(musicString)

end Music


/** Enables the creation of [[Music]] objects via literals such as `music"cdefg"`.*/
extension (noteData: StringContext)
  def music(embeddedBits: Any*) = Music.fromString(noteData.s(embeddedBits*))


/** Represents a piece of music that may consist of multiple [[Voice]]s, which in turn consist
  * of [[MusicElem]]s such as notes.
  *
  * Instead of using the constructor, you can use the [[Music$ companion object]] to construct a
  * `Music` object from a string: e.g., `Music("cdefg&>gfedc")`.
  *
  * @param tempoSetting  a tempo setting (in beats per minute) for the music, if there is one
  * @param voices        the voices that, played simultaneously, make up the piece of music;
  *                      no more than [[MaxVoices]] plus a possible percussion track */
final case class Music(val tempoSetting: Option[Int], val voices: Seq[Voice]) derives CanEqual:

  if tempoSetting.exists( _ <= 0 )                       then throw IllegalArgumentException(s"Tempo needs to be a positive integer.")
  if voices.filterNot( _.isPercussion ).size > MaxVoices then throw IllegalArgumentException(s"A maximum of $MaxVoices tracks in parallel plus percussion track allowed.")
  if voices.count( _.isPercussion ) > 1                  then throw IllegalArgumentException("No more than one percussion track allowed.")

  /** Returns a string description of the music. */
  override def toString =
    val voiceInfo = if this.voices.sizeIs == 1 then voices.head else voices.size.toString + " voices"
    s"$tempo bpm, $voiceInfo"

  /** The tempo of the music in beats per minute. This equals [[tempoSetting]]
    * or [[DefaultTempo]], if that’s not set. */
  lazy val tempo: Int = tempoSetting getOrElse DefaultTempo

  /** whether the `Music` object has any [[MusicElem]]s at all */
  lazy val nonEmpty: Boolean = this.voices.exists( _.nonEmpty )

  /** Plays this music using the MIDI synthesizer. This is equivalent to calling
    * `o1.sound.midi.play`. Students have access to the same functionality via the
    * `play` method in the top-level package [[o1]]. */
  def play(): Unit =
    o1.sound.midi.play(this)

  /** Returns the underlying MIDI sequence. */
  def toMidi: Sequence =
    val seq = Sequence(Sequence.PPQ, PPQ)
    val (percussion, regular) = this.voices.partition( _.isPercussion )
    regular.filter( _.nonEmpty ).foreach( _.addToSequence(seq) )
    percussion.foreach( _.addToSequence(seq) )
    seq

end Music


/** Represents a single voice within a piece of [[Music]]. A voice consists of [[MusicElem]]s,
  * primarily notes, in order.
  *
  * @param notes         the notes (and possible other [[MusicElem]]s) that compose this voice
  * @param isPercussion  whether the notes should not be interpresed as regular
  *                      notes but as the special sounds defined for the
  *                      [[https://web.archive.org/web/20180305180528/https://www.midi.org/specifications/item/gm-level-1-sound-set MIDI standard]]’s
  *                      percussion channel */
final case class Voice(val notes: Seq[MusicElem], val isPercussion: Boolean):
  import ShortMessage.*

  /** Returns a string description of the music. */
  override def toString =
    val preamble = if this.isPercussion then "percussion: " else ""
    preamble + this.notes.mkString(" ")

  /** whether the voice has any [[MusicElem]]s at all */
  lazy val nonEmpty: Boolean = this.notes.nonEmpty

  /** the total `length` (duration) of all the notes in the voice */
  lazy val length: Int = this.notes.map( _.length ).sum

  private[midi] def addToSequence(seq: Sequence): Unit =
    val nextFree    = seq.getTracks.length
    val trackNumber = if this.isPercussion                 then PercussionChannel
                      else if nextFree < PercussionChannel then nextFree
                      else                                      nextFree + 1
    val track       = seq.createTrack()
    track.add(MidiEvent(ShortMessage(STOP), trackPosition(this.length + PPQ / 4)))
    var position      = 0
    var currentOctave = DefaultOctave

    def addEvent(onOrOff: Int, note: Note, position: Int) =
      val message = ShortMessage(onOrOff, trackNumber, note.pitch.midiID(currentOctave), note.volume)
      track.add(MidiEvent(message, trackPosition(position)))

    def processElement(element: MusicElem): Unit = element match
      case note: Note         => addEvent(NOTE_ON, note, position); addEvent(NOTE_OFF, note, position + note.audibleLength)
      case chord: Chord       => chord.notes.foreach(processElement)
      case Instrument(number) => track.add(MidiEvent(ShortMessage(PROGRAM_CHANGE, trackNumber, number - 1, 0), trackPosition(position)))
      case OctaveShift(shift) => currentOctave += shift
      case Pause              => // nothing

    for element <- this.notes do
      processElement(element)
      position += element.length
  end addToSequence

end Voice

