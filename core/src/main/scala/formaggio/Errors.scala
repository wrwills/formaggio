package formaggio

import scalaz._

trait FormError{
  def getErrorMessage(i: String): String
}

trait FormException extends FormError with PimpedType[Exception] {
  def getErrorMessage(i: String) = value.getMessage + ": " + i
}

case object LookupError extends FormError {
  override def getErrorMessage(i: String) = "failed to find value for " + i
}  

case object EmptyError extends FormError {
  override def getErrorMessage(i: String) = i + " must not be empty."
}

case object EmptyStringError extends FormError {
  override def getErrorMessage(i: String) = "empty string not allowed for " + i
}

case class GenericError(msg: String => String) extends FormError {
  override def getErrorMessage(i: String) = msg(i)
}
