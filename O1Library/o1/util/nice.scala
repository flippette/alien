package o1.util

export nice.seq.*
export nice.map.*
export nice.collection.*
export nice.option.tap
export nice.number.*
export nice.pairs.*
export nice.path.*
export nice.regex.*


/** The objects nested herein extend various standard API classes with nice convenience methods. */
object nice:

  /** Provides nice extensions to the interface of `scala.Iterable`, for convenience. */
  object collection:
    extension [Element, Collection[Element] <: Iterable[Element]](self: Collection[Element])

      /** Constructs and returns a new map by applying a key-generating function as well as
        * a value-generating function to each element of the collection. The respective
        * outputs of the functions are paired to form the key—value pairs of the `Map`.
        * @param formKey    a function called on each element of the collection to obtain the keys
        * @param formValue  a function called on each element of the collection to obtain the values */
      def mapify[Key, Value](formKey: Element => Key)(formValue: Element => Value): Map[Key, Value] =
        self.view.map( elem => formKey(elem) -> formValue(elem) ).to(Map)

      /** Constructs and returns a new map by applying an ID-generating function to each
        * element of the collection and using the collection’s elements as values. This is
        * equivalent to calling `mapify(formID)(identity)`.
        * @param formID    a function called on each element of the collection to obtain the keys */
      def mapFromID[ID](formID: Element => ID): Map[ID, Element] =
        self.mapify(formID)(identity)

      /** Constructs and returns a new map by applying a value-generating function
        * to each element of the collection and using the collection’s elements as keys.
        * This is equivalent to calling `mapify(identity)(formValue)`.
        * @param formValue  a function called on each element of the collection to obtain the keys */
      def mapTo[Value](formValue: Element => Value): Map[Element, Value] =
        self.mapify(identity)(formValue)

      /** Constructs and returns a new map by using one given function to group the elements
        * of the collection (as per `groupBy`) and then transforming each of the groups using
        * another given function (as per `mapValues`).
        * @param formKey         a function called on each element of the collection to generate groups of values
        * @param transformGroup  a function called on each group to obtain the values for the resulting collection*/
      def mapGroups[Key, Value](formKey: Element => Key)(transformGroup: Iterable[Element] => Value): Map[Key, Value] =
        self.groupBy(formKey).view.mapValues(transformGroup).to(Map)

      // For comparison, here’s the essence of what the standard API’s groupMap accomplishes:
      // def groupMap[Key, Value](formKey: Element => Key)(formValue: Element => Value): Map[Key, Iterable[Value]] =
      //   self.groupBy(formKey).view.mapValues( _.map(formValue) ).to(Map)

      /** Constructs and returns a new map with the collection’s elements as keys and each element’s
        * occurrence counts as the corresponding values. This is equivalent to `mapGroups(identity)( _.size )`. */
      def frequencies: Map[Element, Int] = self.mapGroups(identity)( _.size )

      /** Performs a given side effect at each element of the collection. Returns the unmodified collection. */
      def tap(effect: Element => Unit): Collection[Element] =
        self.foreach(effect)
        self

      /** Prints out the collection, then returns the unmodified collection.
        * @param format  a function from the collection to the desired printout; defaults to `_.toString` */
      def log(format: Iterable[Element] => String = (_: Iterable[Element]).toString ): Collection[Element] =
        println(format(self))
        self

      /** Returns a `LazyList` containing the collection’s elements. This is equivalent to `to(LazyList)`. */
      def toLazy: LazyList[Element] = self.to(LazyList)

      /** Applies the given function to the collection in case there’s at least one element there.
        * Returns `None` if the collection is empty. */
      inline def ifNonEmpty[Result](compute: Collection[Element] => Result): Option[Result] =
        if self.nonEmpty then Some(compute(self)) else None

      /** Returns an iterator of pairs (tuples) that “slide across” the collection as per `sliding`. */
      def slidingPairs: Iterator[(Element, Element)] =
        self.sliding(2).map( seq2 => (seq2.head, seq2.last) )

  end collection


  /** Provides nice extensions to the interface of `scala.Seq`, for convenience. */
  object seq:
    extension [Element](self: Seq[Element])
      /** Return a random element from the collection. Uses the standard `scala.util.Random` singleton. */
      def randomElement(): Element =
        self.randomElement(scala.util.Random)
      /** Return a random element from the collection. Uses the given `Random` instance. */
      def randomElement(randomizer: scala.util.Random): Element =
        val randomIndex = randomizer.nextInt(self.size)
        self(randomIndex)
  end seq


  /** Provides nice extensions to the interface of `scala.Map`, for convenience. */
  object map:
    extension [Key, Value](self: Map[Key, Value])
      /** Constructs and returns a new map by transforming each value with a given function.
        * @param transform the value-transforming function */
      def mapValuesOnly[Result](transform: Value => Result): Map[Key, Result] =
        self.map( (key, value) => key -> transform(value) )
  end map


  /** Provides nice extensions to the interface of `scala.Option`, for convenience. */
  object option:
    extension [Content](self: Option[Content])

      /** Performs a given side effect on the `Option`’s contents (if any). Returns the `Option` as is. */
      def tap(effect: Content => Unit): Option[Content] =
        self.foreach(effect)
        self

      /** Prints out a report of the `Option`’s contents (if any). Returns the unmodified `Option`.
        * @param format           a function that takes in a description of an `Option` and produces a report of it; defaults to `identity`
        * @param describeContent  a function that takes in an `Option`’s contents and produces a description of it; defaults to `_.toString`
        * @param describeNone     a description of an empty `Option`; defaults to `"None"` */
      def log(format: String => String = identity, describeContent: Content => String = (_: Content).toString, describeNone: =>String = "None"): Option[Content] =
        if self.isDefined then self.tap( describeContent andThen format andThen println )
        else
          println(format(describeNone))
        self

  end option


  /** Provides nice extensions to the interface of basic numerical types, for convenience. */
  object number:

    extension (value: Int)
      /** `num atLeast limit` is equivalent to `num.max(limit)`. */
      inline infix def atLeast(minimum: Int): Int = value.max(minimum)
      /** `num atMost limit` is equivalent to `num.min(limit)`. */
      inline infix def atMost(maximum: Int): Int = value.min(maximum)
      /** `num.clamp(low, high)` is equivalent to `num.max(low).min(high)`. */
      inline def clamp(low: Int, high: Int): Int = value atLeast low atMost high
      /** Determines if the at least as large as `low` and less than `high`. Note that the lower bound is inclusive and the upper bound exclusive.  */
      inline def isBetween(low: Int, high: Int): Boolean = value >= low && value < high
      /** Determines if the integer isn’t divisible by two. */
      inline def isOdd: Boolean = value % 2 != 0
      /** Determines if the integer is divisible by two. */
      inline def isEven: Boolean = value % 2 == 0

      /** `myInt1 ddiv myInt2 is equivalent to `myInt1 / myInt2.toDouble`. */
      inline infix def ddiv(divisor: Int): Double = value / divisor.toDouble

    extension (value: Double)
      /** `num atLeast limit` is equivalent to `num.max(limit)`. */
      inline infix def atLeast(minimum: Double): Double = value.max(minimum)
      /** `num atMost limit` is equivalent to `num.min(limit)`. */
      inline infix def atMost(maximum: Double): Double = value.min(maximum)
      /** `num.clamp(low, high)` is equivalent to `num.max(low).min(high)`. */
      inline def clamp(low: Double, high: Double): Double = value atLeast low atMost high
      /** Determines if the at least as large as `low` and less than `high`. Note that the lower bound is inclusive and the upper bound exclusive.  */
      inline def isBetween(low: Double, high: Double): Boolean = value >= low && value < high

    extension (value: Float)
      /** `num atLeast limit` is equivalent to `num.max(limit)`. */
      inline infix def atLeast(minimum: Float): Float = value.max(minimum)
      /** `num atMost limit` is equivalent to `num.min(limit)`. */
      inline infix def atMost(maximum: Float): Float = value.min(maximum)
      /** `num.clamp(low, high)` is equivalent to `num.max(low).min(high)`. */
      inline def clamp(low: Float, high: Float): Float = value atLeast low atMost high
      /** Determines if the at least as large as `low` and less than `high`. Note that the lower bound is inclusive and the upper bound exclusive.  */
      inline def isBetween(low: Float, high: Float): Boolean = value >= low && value < high

    extension (value: Long)
      /** `num atLeast limit` is equivalent to `num.max(limit)`. */
      inline infix def atLeast(minimum: Long): Long = value.max(minimum)
      /** `num atMost limit` is equivalent to `num.min(limit)`. */
      inline infix def atMost(maximum: Long): Long = value.min(maximum)
      /** `num.clamp(low, high)` is equivalent to `num.max(low).min(high)`. */
      inline def clamp(low: Long, high: Long): Long = value atLeast low atMost high
      /** Determines if the at least as large as `low` and less than `high`. Note that the lower bound is inclusive and the upper bound exclusive.  */
      inline def isBetween(low: Long, high: Long): Boolean = value >= low && value < high
      /** Determines if the integer isn’t divisible by two. */
      inline def isOdd: Boolean = value % 2 != 0
      /** Determines if the integer is divisible by two. */
      inline def isEven: Boolean = value % 2 == 0

    def min(ints: Int*) = ints.min
    def min(doubles: Double*) = doubles.min
    def max(ints: Int*) = ints.min
    def max(doubles: Double*) = doubles.min

    def ascending(a: Int, b: Int): (Int, Int) =
      if a <= b then (a, b) else (b, a)

    def ascending(a: Double, b: Double): (Double, Double) =
      if a <= b then (a, b) else (b, a)

  end number


  /** Provides nicer-named methods for accessing members of pairs. */
  object pairs:
    extension [T1,T2](self: Tuple2[T1, T2])
      /** Returns the first member of a pair. */
      inline def first: T1 = self(0)
      /** Returns the first second member of a pair. */
      inline def second: T2 = self(1)

    extension [T1, T2, Pair <: Tuple2[T1,T2], Collection[Pair] <: Seq[Pair]](self: Collection[Tuple2[T1, T2]])
      /** Returns a new collection with the first elements of each pair from this one. */
      inline def firsts: Seq[T1] = self.map[T1]( _(0) )
      /** Returns a new collection with the first elements of each pair from this one. */
      inline def seconds: Seq[T2] = self.map[T2]( _(1) )
  end pairs


  /** Provides nicer-named methods for manipulating paths. */
  object path:
    export o1.util.pathOps.*

  /** Provides nicer tools for manipulating regular expressions. */
  private[o1] object regex:
    import scala.util.matching.Regex
    extension (interpolated: StringContext)
      def r = Regex(interpolated.parts.mkString, interpolated.parts.tail.map( _ => "unnamedGroup" )*)

end nice

