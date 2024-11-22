/** This package contains [[Color]] constants. They include the colors listed in the W3C’s
  * [[https://www.w3.org/TR/css-color-4/ CSS Color Module specification]] (version December,
  * 2021), plus some additional ones.
  *
  * All the colors are fully opaque, except [[colors.Transparent Transparent]], which is
  * fully transparent.
  *
  * The contents of this package are aliased in the top-level package [[o1]] so that
  * they are accessible to students simply via `import o1.*`. */
package o1.gui.colors

import o1.gui.Color

/** Represents a fully transparent (white) color. */
val Transparent: Color = Color(0x00ffffff, "transparent")

/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val AliceBlue: Color = Color(0xfff0f8ff, "alice blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val AlizarinCrimson: Color = Color(0xff4e1500, "alizarin crimson")
/** A named color as per, um, some standard or other, maybe. */
val Amethyst: Color = Color(0xff9966cc, "amethyst")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val AntiqueWhite: Color = Color(0xfffaebd7, "antique white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Aqua: Color = Color(0xff00ffff, "aqua")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Aquamarine: Color = Color(0xff7fffd4, "aquamarine")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Azure: Color = Color(0xfff0ffff, "azure")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Beige: Color = Color(0xfff5f5dc, "beige")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Bisque: Color = Color(0xffffe4c4, "bisque")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Black: Color = Color(0xff000000, "black")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val BlanchedAlmond: Color = Color(0xffffebcd, "blanched almond")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Blue: Color = Color(0xff0000ff, "blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val BlueViolet: Color = Color(0xff8a2be2, "blue violet")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val BrightRed: Color = Color(0xffdb0000, "bright red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Brown: Color = Color(0xffa52a2a, "brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val BurlyWood: Color = Color(0xffdeb887, "burly wood")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val CadetBlue: Color = Color(0xff5f9ea0, "cadet blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val CadmiumYellow: Color = Color(0xffffec00, "cadmium yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Chartreuse: Color = Color(0xff7fff00, "chartreuse")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Chocolate: Color = Color(0xffd2691e, "chocolate")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Coral: Color = Color(0xffff7f50, "coral")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val CornflowerBlue: Color = Color(0xff6495ed, "cornflower blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Cornsilk: Color = Color(0xfffff8dc, "cornsilk")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Crimson: Color = Color(0xffdc143c, "crimson")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Cyan: Color = Color(0xff00ffff, "cyan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkBlue: Color = Color(0xff00008b, "dark blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkCyan: Color = Color(0xff008b8b, "dark cyan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGoldenrod: Color = Color(0xffb8860b, "dark goldenrod")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGray: Color = Color(0xffa9a9a9, "dark gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGreen: Color = Color(0xff006400, "dark green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGrey: Color = Color(0xffa9a9a9, "dark grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkKhaki: Color = Color(0xffbdb76b, "dark khaki")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkMagenta: Color = Color(0xff8b008b, "dark magenta")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkOliveGreen: Color = Color(0xff556b2f, "dark olive green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkOrange: Color = Color(0xffff8c00, "dark orange")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkOrchid: Color = Color(0xff9932cc, "dark orchid")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkRed: Color = Color(0xff8b0000, "dark red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSalmon: Color = Color(0xffe9967a, "dark salmon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSeaGreen: Color = Color(0xff8fbc8f, "dark sea green")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val DarkSienna: Color = Color(0xff5f2e1f, "dark sienna")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSlateBlue: Color = Color(0xff483d8b, "dark slate blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSlateGray: Color = Color(0xff2f4f4f, "dark slate gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSlateGrey: Color = Color(0xff2f4f4f, "dark slate grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkTurquoise: Color = Color(0xff00ced1, "dark turquoise")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkViolet: Color = Color(0xff9400d3, "dark violet")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DeepPink: Color = Color(0xffff1493, "deep pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DeepSkyBlue: Color = Color(0xff00bfff, "deep sky blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DimGray: Color = Color(0xff696969, "dim gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DimGrey: Color = Color(0xff696969, "dim grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DodgerBlue: Color = Color(0xff1e90ff, "dodger blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val FireBrick: Color = Color(0xffb22222, "fire brick")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val FloralWhite: Color = Color(0xfffffaf0, "floral white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val ForestGreen: Color = Color(0xff228b22, "forest green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Fuchsia: Color = Color(0xffff00ff, "fuchsia")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Gainsboro: Color = Color(0xffdcdcdc, "gainsboro")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val GhostWhite: Color = Color(0xfff8f8ff, "ghost white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Gold: Color = Color(0xffffd700, "gold")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Goldenrod: Color = Color(0xffdaa520, "goldenrod")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Gray: Color = Color(0xff808080, "gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Green: Color = Color(0xff008000, "green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val GreenYellow: Color = Color(0xffadff2f, "green yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Grey: Color = Color(0xff808080, "grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Honeydew: Color = Color(0xfff0fff0, "honeydew")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val HotPink: Color = Color(0xffff69b4, "hot pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val IndianRed: Color = Color(0xffcd5c5c, "indian red")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val IndianYellow: Color = Color(0xffffb800, "indian yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Indigo: Color = Color(0xff4b0082, "indigo")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Ivory: Color = Color(0xfffffff0, "ivory")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Khaki: Color = Color(0xfff0e68c, "khaki")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Lavender: Color = Color(0xffe6e6fa, "lavender")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LavenderBlush: Color = Color(0xfffff0f5, "lavender blush")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LawnGreen: Color = Color(0xff7cfc00, "lawn green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LemonChiffon: Color = Color(0xfffffacd, "lemon chiffon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightBlue: Color = Color(0xffadd8e6, "light blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightCoral: Color = Color(0xfff08080, "light coral")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightCyan: Color = Color(0xffe0ffff, "light cyan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGoldenrodYellow: Color = Color(0xfffafad2, "light goldenrod yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGray: Color = Color(0xffd3d3d3, "light gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGreen: Color = Color(0xff90ee90, "light green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGrey: Color = Color(0xffd3d3d3, "light grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightPink: Color = Color(0xffffb6c1, "light pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSalmon: Color = Color(0xffffa07a, "light salmon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSeaGreen: Color = Color(0xff20b2aa, "light sea green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSkyBlue: Color = Color(0xff87cefa, "light sky blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSlateGray: Color = Color(0xff778899, "light slate gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSlateGrey: Color = Color(0xff778899, "light slate grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSteelBlue: Color = Color(0xffb0c4de, "light steel blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightYellow: Color = Color(0xffffffe0, "light yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Lime: Color = Color(0xff00ff00, "lime")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LimeGreen: Color = Color(0xff32cd32, "lime green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Linen: Color = Color(0xfffaf0e6, "linen")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Magenta: Color = Color(0xffff00ff, "magenta")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Maroon: Color = Color(0xff800000, "maroon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumAquamarine: Color = Color(0xff66cdaa, "medium aquamarine")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumBlue: Color = Color(0xff0000cd, "medium blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumOrchid: Color = Color(0xffba55d3, "medium orchid")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumPurple: Color = Color(0xff9370db, "medium purple")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumSeaGreen: Color = Color(0xff3cb371, "medium sea green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumSlateBlue: Color = Color(0xff7b68ee, "medium slate blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumSpringGreen: Color = Color(0xff00fa9a, "medium spring green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumTurquoise: Color = Color(0xff48d1cc, "medium turquoise")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumVioletRed: Color = Color(0xffc71585, "medium violet red")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val MidnightBlack: Color = Color(0xff000000, "midnight black")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MidnightBlue: Color = Color(0xff191970, "midnight blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MintCream: Color = Color(0xfff5fffa, "mint cream")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MistyRose: Color = Color(0xffffe4e1, "misty rose")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Moccasin: Color = Color(0xffffe4b5, "moccasin")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val NavajoWhite: Color = Color(0xffffdead, "navajo white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Navy: Color = Color(0xff000080, "navy")
/** A named color as per [[http://montypython.50webs.com/Smileipts/Series_1/53.htm the MP standard]]. */
lazy val NorwegianBlue: Color = throw NotImplementedError("This color has expired.")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val OldLace: Color = Color(0xfffdf5e6, "old lace")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Olive: Color = Color(0xff808000, "olive")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val OliveDrab: Color = Color(0xff6b8e23, "olive drab")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Orange: Color = Color(0xffffa500, "orange")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val OrangeRed: Color = Color(0xffff4500, "orange red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Orchid: Color = Color(0xffda70d6, "orchid")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleGoldenrod: Color = Color(0xffeee8aa, "pale goldenrod")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleGreen: Color = Color(0xff98fb98, "pale green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleTurquoise: Color = Color(0xffafeeee, "pale turquoise")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleVioletRed: Color = Color(0xffdb7093, "pale violet red")
/** A named color [[https://en.wikipedia.org/wiki/Pantone_448_C famous  for  being  really  ugly]]. */
val Pantone448C: Color = Color(0xff4a412a, "Pantone 448 C")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PapayaWhip: Color = Color(0xffffefd5, "papaya whip")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PeachPuff: Color = Color(0xffffdab9, "peach puff")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Peru: Color = Color(0xffcd853f, "peru")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val PhthaloBlue: Color = Color(0xff0c0040, "phthalo blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val PhthaloGreen: Color = Color(0xff102e3c, "phthalo green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Pink: Color = Color(0xffffc0cb, "pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Plum: Color = Color(0xffdda0dd, "plum")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PowderBlue: Color = Color(0xffb0e0e6, "powder blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val PrussianBlue: Color = Color(0xff021e44, "Prussian blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Purple: Color = Color(0xff800080, "purple")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val RebeccaPurple: Color = Color(0xff663399, "rebecca purple")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Red: Color = Color(0xffff0000, "red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val RosyBrown: Color = Color(0xffbc8f8f, "rosy brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val RoyalBlue: Color = Color(0xff4169e1, "royal blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SaddleBrown: Color = Color(0xff8b4513, "saddle brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Salmon: Color = Color(0xfffa8072, "salmon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SandyBrown: Color = Color(0xfff4a460, "sandy brown")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val SapGreen: Color = Color(0xff0a3410, "sap green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SeaGreen: Color = Color(0xff2e8b57, "sea green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Seashell: Color = Color(0xfffff5ee, "seashell")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Sienna: Color = Color(0xffa0522d, "sienna")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Silver: Color = Color(0xffc0c0c0, "silver")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SkyBlue: Color = Color(0xff87ceeb, "sky blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SlateBlue: Color = Color(0xff6a5acd, "slate blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SlateGray: Color = Color(0xff708090, "slate gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SlateGrey: Color = Color(0xff708090, "slate grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Snow: Color = Color(0xfffffafa, "snow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SpringGreen: Color = Color(0xff00ff7f, "spring green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SteelBlue: Color = Color(0xff4682b4, "steel blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Tan: Color = Color(0xffd2b48c, "tan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Teal: Color = Color(0xff008080, "teal")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Thistle: Color = Color(0xffd8bfd8, "thistle")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val TitaniumHwite: Color = Color(0xffffffff, "titanium hwite")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Tomato: Color = Color(0xffff6347, "tomato")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Turquoise: Color = Color(0xff40e0d0, "turquoise")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val VanDykeBrown: Color = Color(0xff221b15, "Van Dyke brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Violet: Color = Color(0xffee82ee, "violet")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Wheat: Color = Color(0xfff5deb3, "wheat")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val White: Color = Color(0xffffffff, "white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val WhiteSmoke: Color = Color(0xfff5f5f5, "white smoke")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Yellow: Color = Color(0xffffff00, "yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val YellowGreen: Color = Color(0xff9acd32, "yellow green")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val YellowOchre: Color = Color(0xffc79b00, "yellow ochre")


/** A grayscale color that contains 10% black. */
val Black10: Color = Black.lighter(90).withName("black10")
/** A grayscale color that contains 20% black. */
val Black20: Color = Black.lighter(80).withName("black20")
/** A grayscale color that contains 30% black. */
val Black30: Color = Black.lighter(70).withName("black30")
/** A grayscale color that contains 40% black. */
val Black40: Color = Black.lighter(60).withName("black40")
/** A grayscale color that contains 50% black. */
val Black50: Color = Black.lighter(50).withName("black50")
/** A grayscale color that contains 60% black. */
val Black60: Color = Black.lighter(40).withName("black60")
/** A grayscale color that contains 70% black. */
val Black70: Color = Black.lighter(30).withName("black70")
/** A grayscale color that contains 80% black. */
val Black80: Color = Black.lighter(20).withName("black80")
/** A grayscale color that contains 90% black. */
val Black90: Color = Black.lighter(10).withName("black90")
/** A grayscale color that contains 10% white. */
val White10: Color = White.darker(90).withName("white10")
/** A grayscale color that contains 20% white. */
val White20: Color = White.darker(80).withName("white20")
/** A grayscale color that contains 30% white. */
val White30: Color = White.darker(70).withName("white30")
/** A grayscale color that contains 40% white. */
val White40: Color = White.darker(60).withName("white40")
/** A grayscale color that contains 50% white. */
val White50: Color = White.darker(50).withName("white50")
/** A grayscale color that contains 60% white. */
val White60: Color = White.darker(40).withName("white60")
/** A grayscale color that contains 70% white. */
val White70: Color = White.darker(30).withName("white70")
/** A grayscale color that contains 80% white. */
val White80: Color = White.darker(20).withName("white80")
/** A grayscale color that contains 90% white. */
val White90: Color = White.darker(10).withName("white90")

