/** This is one of O1’s sound packages (the other being [[o1.sound.midi]]). This package
  * provides a simple interface for loading recorded sound samples.
  *
  * The contents of the package have aliases in the top-level package [[o1]], so they are
  * accessible to students simply via `import o1.*`. */
package o1.sound.sampled

import javax.sound.sampled.Clip

/** a constant that you can pass to [[Sound.play play]] to make the sound repeat indefinitely */
val KeepRepeating: Int = Clip.LOOP_CONTINUOUSLY

/** a constant that you can pass to various methods in this package to silence a sound sample completely */
val Mute: Float = Float.MinValue

