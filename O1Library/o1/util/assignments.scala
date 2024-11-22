package o1.util

import o1.gui.Dialog.*
import o1.util.*

import java.lang.reflect.{Constructor, InvocationTargetException}
import scala.language.dynamics
import scala.reflect.ClassTag
import scala.swing.Component

private[o1] object assignments:

  // utils for reflection:

  private[o1] final class Argument(val value: Any) extends AnyVal:
    override def toString = s"arg($value)"

  private[o1] def arg(any: Matchable) = Argument(javify(any))

  private def javify(scalaVal: Matchable) = scalaVal match
    case int: Int         => java.lang.Integer.valueOf(int)
    case double: Double   => java.lang.Double.valueOf(double)
    case char: Char       => java.lang.Character.valueOf(char)
    case boolean: Boolean => java.lang.Boolean.valueOf(boolean)
    case short: Short     => java.lang.Short.valueOf(short)
    case byte: Byte       => java.lang.Byte.valueOf(byte)
    case float: Float     => java.lang.Float.valueOf(float)
    case long: Long       => java.lang.Long.valueOf(long)
    case otherObject      => otherObject.asInstanceOf[AnyRef]



  // Wrappers for the “dynamic” use of evolving and buggy student classes, implemented using Java reflection.
  // Upgrade to Scala reflection later.

  private[o1] open class DynamicClass[Supertype <: AnyRef](private val className: String, private val constructorParameterTypes: Seq[Class[?]]):

    private val actualClass = Try(Class.forName(className).asInstanceOf[Class[Supertype]]) recoverWith {
      case noSuchClass: ClassNotFoundException => println(s"The class $className was not available."); throw noSuchClass
      case invalidClass: ClassCastException    => invalidClass.printStackTrace(); throw invalidClass
    }

    val isUsable =
      val constructor: Try[Constructor[?]] =
        Try(Class.forName(className).asInstanceOf[Class[Supertype]].getConstructor(constructorParameterTypes*))
      constructor.isSuccess

    def instantiate(parameters: Argument*): Supertype = this.actualClass match
      case Failure(_) => throw InvalidSignature(s"The class $className is not available and wasn't successfully instantiated.")
      case Success(actualClass) =>
        try
          val constructor = actualClass.getConstructor(this.constructorParameterTypes*)
          constructor.newInstance( (parameters.map( _.value ))*)
        catch
          case problemWithinImplementation: InvocationTargetException =>
            println(s"The instantiation of class $className failed to complete.")
            throw problemWithinImplementation
          case instantiationProblem: Exception =>
            throw InvalidSignature(s"The class $className wasn't successfully instantiated.")

  end DynamicClass


  private[o1] open class DynamicObject[StaticType](private val wrapped: StaticType) extends Dynamic:

    def applyDynamic[ResultType: ClassTag](methodName: String)(parameters: (Class[?], Argument)*): ResultType =
      val returnValue = try
        val method = this.wrapped.getClass.getMethod(methodName, parameters.map( _.first )*)
        method.invoke(this.wrapped, parameters.map( _.second.value )*)
      catch
        case problemWithinImplementation: InvocationTargetException =>
          println(s"A call to the method $methodName failed to complete.")
          throw problemWithinImplementation
        case otherProblem: Exception =>
          throw InvalidSignature(s"The method or variable $methodName was not successfully accessed.")

      val boxings = Map[Class[?], Class[?]](classOf[Boolean] -> classOf[java.lang.Boolean], classOf[Int] -> classOf[java.lang.Integer], classOf[Double] -> classOf[java.lang.Double], classOf[Char] -> classOf[java.lang.Character], classOf[Short] -> classOf[java.lang.Short], classOf[Long] -> classOf[java.lang.Long], classOf[Byte] -> classOf[java.lang.Byte], classOf[Float] -> classOf[java.lang.Float])
      val expectedClassTag = implicitly[ClassTag[ResultType]]
      val expectedClass = expectedClassTag.runtimeClass
      given CanEqual[ClassTag[?], ClassTag[Unit]] = CanEqual.derived
      if expectedClassTag == ClassTag(classOf[Unit]) || expectedClass.isInstance(returnValue) ||
                             boxings.get(expectedClass).exists( _.isInstance(returnValue) ) then
        returnValue.asInstanceOf[ResultType]
      else
        throw InvalidSignature(s"The return value of $methodName was not of the expected type.")
    end applyDynamic

    def selectDynamic[ResultType](methodName: String)(implicit expectedClassTag: ClassTag[ResultType]): ResultType =
      this.applyDynamic[ResultType](methodName)()

    def get[StaticType] = this.wrapped

  end DynamicObject


  private[o1] case class InvalidSignature(val message: String) extends Exception(message)


  private[o1] trait RequestArguments[T <: AnyRef]:
    this: DynamicClass[T] =>

    protected val argumentRequesters: Seq[() => Option[Matchable]]

    def requestInstance(): Option[T] =
      val responses: LazyList[Matchable] =
        this.argumentRequesters.to(LazyList).map( _() ).takeWhile( _.isDefined ).flatten
      if responses.size < argumentRequesters.size then None else Some(this.instantiate(responses.map(arg)*))

  end RequestArguments


  private[o1] def withStudentSolution(owner: Component)(task: => Unit): Unit =
    try task
    catch
      case studentCodeProducedException: InvocationTargetException =>
        throw studentCodeProducedException
      case InvalidSignature(solutionDoesntMeetSpecification) =>
        println(solutionDoesntMeetSpecification)
        display(s"A part of the implementation was missing or invalid: $solutionDoesntMeetSpecification", RelativeTo(owner))

end assignments