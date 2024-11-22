package o1.gui.font

import java.awt.GraphicsEnvironment

/** The names of all the fonts available in the graphics environment where the code is running.
  * (Examples: "Times New Roman", "Times New Roman Italic", "Arial", "Arial Bold", Comic Sans MS".) */
lazy val allFontNames: Seq[String] =
  GraphicsEnvironment.getLocalGraphicsEnvironment.getAllFonts.map( _.getName ).toSeq

/** The names of all the font families available in the graphics environment where the code is running.
  * (E.g., "Serif", "SansSerif", "Monospaced") */
lazy val allFontFamilyNames: Seq[String] =
  GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toSeq

