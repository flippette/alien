package o1.util

// Aliases to the standard API, for convenience.

/** An alias for convenient use of `scala.util.Try` via `import o1.util.*`. */
type Try[T] = scala.util.Try[T]
/** An alias for convenient use of `scala.util.Try` via `import o1.util.*`. */
val Try = scala.util.Try


/** An alias for convenient use of `scala.util.Try` via `import o1.util.*`. */
type Success[T] = scala.util.Success[T]
/** An alias for convenient use of `scala.util.Try` via `import o1.util.*`. */
val Success = scala.util.Success


/** An alias for convenient use of `scala.util.Try` via `import o1.util.*`. */
type Failure[T] = scala.util.Failure[T]
/** An alias for convenient use of `scala.util.Try` via `import o1.util.*`. */
val Failure = scala.util.Failure


/** An alias for convenient use of `scala.util.Random` via `import o1.util.*`. */
type Random = scala.util.Random
/** An alias for convenient use of `scala.util.Random` via `import o1.util.*`. */
val Random = scala.util.Random


/** An alias for convenient use of `scala.math.Ordering.Double.TotalOrdering` via `import o1.util.*`. */
val DoubleOrdering = scala.math.Ordering.Double.TotalOrdering


/** An alias for convenient use of `java.net.URL` via `import o1.util.*`. */
type URL = java.net.URL

/** An alias for convenient use of `java.nio.file.Path` via `import o1.util.*`. */
type Path = java.nio.file.Path

/** An alias for convenient use of `scala.io.Source` via `import o1.util.*`. */
type Source = scala.io.Source
given CanEqual[Source, Source] = CanEqual.derived
/** An alias for convenient use of `scala.io.Source` via `import o1.util.*`. */
val Source = scala.io.Source


/** An alias for convenient access to `java.lang.System.getProperty("user.dir")` via `import o1.util.*`.*/
def workingDir = System.getProperty("user.dir")

