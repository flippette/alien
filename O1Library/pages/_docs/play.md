---
title: Using o1.play
---

# Using `o1.play` for MIDI Sound

The function `o1.play` plays MIDI music given as `String`s that describe notes. 
Below is a summary of the notation used in the `String`s that you can pass to `play`.

- `"cdefgah"`                plays seven notes at the default tempo of 120. **(N.B. the seventh note is `h`, not `b`.)**
- `"CDEFGAH"`                plays them louder.
- `"CDEFGAH/240"`            plays them at a double tempo of 240.
- `"CD E"`                   has a pause between the second and third note.
- `"CD-E---"`                has a longer second note and a longer still third note.
- `"C.D.E"`                  produces a staccato-like effect on the first two notes (playing them shorter followed by a pause).
- `">CDE<<<CDE"`             plays three notes in a higher octave then shifts three octaves down before playing them again.
- `"C7D3E"`                  plays the c in Octave #7, the d in Octave #3, and the e in the default Octave #5.
- `"CbDBE#7F"`               has a c-flat, a d-flat, an e-sharp in Octave #7, and a natural f. b and B are equivalent.
- `"C♭D♭E♯7F♮"`              is a fancy-pants way of writing the same thing.
- `"CDE[13]CDE"`             plays three notes using the default Instrument #1, then again using Instrument #13.
- `"(CEG)(DF#A)(EG#H)---"`   plays three chords, the last of which is longer.
- `"CDE&<<[28]efg&[110]  F"` simultaneously plays the three parts separated by `&`s.
- `"P:CDE"`                  uses the MIDI percussion channel: each "note"` represents a different percussion instrument.
- `"C|D||||E"`               means the same as `"cde"`: the `|`s don’t do anything, but you can use them to mark bars or whatever.

For a numbered list of the instruments, see [[the General MIDI Sound Set|https://web.archive.org/web/20180305180528/https://www.midi.org/specifications/item/gm-level-1-sound-set]]. 
The [[Instrument|o1.sound.midi.Instrument$]] object contains the same list as Scala constants.

*(Side note: The content on this page will eventually be moved to the API description of 
package `o1.sound.midi`, but it’s here while Scaladoc3 doesn’t properly support top-level 
documentation for packages.)*
