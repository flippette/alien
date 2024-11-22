package o1.sound.midi

import scala.util.Try

/** This object is an interface to the underlying MIDI sequencer. It is a thin Scala wrapper
  * around the [[javax.sound.midi.Sequencer sequencer in the Java MIDI API]]. */
object Sequencer:

  /** the underlying Java MIDI sequencer */
  lazy val peer: javax.sound.midi.Sequencer =
    def failOnError =
      val message = "The system does not seem to support MIDI sound, or sound support is disabled."
      throw MidiUnavailableException(message)
    Try(javax.sound.midi.MidiSystem.getSequencer) getOrElse failOnError

  /** Prepares the sequencer for use. This method needs to be called before
    * calling [[start]] for the first time. */
  def open(): Unit =
    import javax.sound.midi.MetaMessage
    this.peer.open()
    //noinspection AutoTupling
    this.peer.addMetaEventListener((message: MetaMessage) => if message.getType == 0x2F then peer.close() )

  /** Frees up resources associated with the sequencer’s Java peer. */
  def close(): Unit =
    this.peer.close()

  /** Starts playing the given musical sequence.
    *
    * You must call [[open]] once before playing any music with this method,
    * or an `IllegalStateException` will be thrown.
    *
    * @param music         the music to play
    * @param initialDelay  an initial delay, in milliseconds, to allow for the synthesizer’s latency. Defaults to `Synthesizer.latency / 1000`. */
  def start(music: Music, initialDelay: Long = Synthesizer.latency / 1000): Unit =
    this.peer.setSequence(music.toMidi)
    this.peer.setTempoInBPM(music.tempo.toFloat)
    Thread.sleep(initialDelay)
    this.peer.start()

end Sequencer

