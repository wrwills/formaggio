package scormlets

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

case object EmptyStringError extends FormError {
  override def getErrorMessage(i: String) = "empty string not allowed for " + i
}

case class GenericError(msg: String => String) extends FormError {
  override def getErrorMessage(i: String) = msg(i)
}


/**
 * Formlets
 */
object Formlets {

  import Scalaz._
  import FromString._

  import scala.xml.NodeSeq
  import scala.xml.Text
  import Applicative._
  import Pointed._

  // allows us to treat exceptions arising from eg parsing errors as form validation
  // errors  
  //implicit 
  def ExceptionTo(e: Exception): FormException = new FormException {
    val value = e
  } 

  //def liftFormErrorValidationToFormValidation[A](v: Validation[FormError,A]): 

  def addIndexToValidation[A](i: String, v: Validation[FormError,A]): Validation[(String,FormError),A] =
    v match {
      case Success(s) => s.success
      case Failure(f) => failure(i, f)
    }     
  
  //case class LookupException(e: String) extends Exception(e)
  
  def liftExceptionValidation[A](v: Validation[Exception,A]): Validation[(String,Exception),A] =
    v match {
      case Success(s) => s.success[(String,Exception)]
      case Failure(f) => failure[(String,Exception),A]("", f)
    } 
  /*
    v match {
      case Success(s) => success[(String,String),A](s)
      case Failure(f) => f match {
	case LookupException =>  failure[(String,String),A](e, " can not be empty")
	case _                =>  failure[(String,String),A]("", f.toString)
      }
    } */

  type Errors = Map[String,String]
  //type Errors = Map[String,Exception]
  type Env = Map[String, String] // worry about files later
  //type ValidForm[A] = Validation[NonEmptyList[(String,String)],A]
  type ValidForm[A] = Validation[NonEmptyList[(String,FormError)],A]

  // form state is an integer showing the number of the last input together 
  type FormState = (Int,List[String])

  /*
   * A view represents a visual representation of a form. It is a
   * function which takes a list of all errors and then produces a new view
   */
  type View = Errors => NodeSeq

  trait Form[A] extends NewType[ Env => State[FormState,(ValidForm[A],View)] ] {

    /*
     * Append a unit form to the left. This is useful for adding labels or error
     * fields
     * only works if current form returns Unit
     * Evaluate the form that matters first, so we have a correct range set 
     */    
    def ++>[B](frm: Form[B]): Form[B] = 
      Form((env: Env) => 
	for {
	     frslt <- frm.value(env)
	     s <- init[FormState]
	     arslt <- this.value(env)
	   } yield
	     (frslt._1, arslt._2 ⊹ frslt._2))
	 
    /*
     * Append a unit form to the right. See '++>'.
     */    
    def <++(frm: Form[Unit]): Form[A] = 
      Form((env: Env) => 
	for {
	  s <- init[FormState]
	  arslt <- this.value(env)
	  frslt <- frm.value(env)
	} yield 
	  (arslt._1, arslt._2 ⊹ frslt._2))


  }
      

  object Form {
    def apply[A](fn: Env => State[FormState,(ValidForm[A],View)]) = 
      new Form[A]{ val value = fn }
  }

  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) =
      Form((env: Env) => 
	for {s <- init[FormState]} 
	yield (success(a), (errors: Map[String,String]) => Text("")))
  }

  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] = 
      Form((env: Env) => 
	for {s <- init[FormState];
	     rslt <-r.value(env) 
	   } yield rslt  match {
	     case (Success(a),view) => (success(f(a)),view)
	     case (Failure(e),view) => (failure(e),view)
	   })
   }	   

  implicit def FormApply: Apply[Form] = new Apply[Form] {
    def apply[A,B](f: => Form[A => B], a: => Form[A]): Form[B] = 
      Form(
	(env: Env) =>
	for {s <- init[FormState];
	     frslt <- f.value(env); 
	     arslt <- a.value(env)
	   } yield {
	     val valid = (frslt._1,arslt._1)  match {
	       case (Success(x),Success(y)) => success(x(y))
	       case (Success(x),Failure(y)) => failure(y)
	       case (Failure(x),Success(y)) => failure(x)
	       case (Failure(x),Failure(y)) => failure(x ⊹ y)
	     }
	     (valid, frslt._2 ⊹ arslt._2)
	   })
  }

  /*
  def liftValidForm(rslt: Validation[Exception,A]): ValidForm[A] =
    rslt match {
      case Success(x) => x match {
	      case Success(xx) => success[NonEmptyList[(String,Exception)],A](xx)
	      case Failure(xy) => failure[NonEmptyList[(String,Exception)],A](
		NonEmptyList( (s._2.headOption.getOrElse("unknown"), xy ) ))
	    }
      case Failure(y) => failure[NonEmptyList[(String,Exception)],A](y)
    }

  def liftValidForm(rslt: Validation[String,A]): ValidForm[A] =
    rslt match {
      case Success(x) => x match {
	      case Success(xx) => success[NonEmptyList[(String,Exception)],A](xx)
	      case Failure(xy) => failure[NonEmptyList[(String,Exception)],A](
		NonEmptyList( (s._2.headOption.getOrElse("unknown"), xy ) ))
	    }
      case Failure(y) => failure[NonEmptyList[(String,Exception)],A](y)
    } */

//  def convertFailure[A](f: Failure[Exception, A]): Failure[NonEmptyList

  def validate[A](form: Form[Validation[String,A]]): Form[A] = 
    validate(form, (x:String) => GenericError((_: String) => x) )

  /*
  def validate[A](form: Form[Validation[Exception,A]]): Form[A] = 
    validate(form, (x: Exception) => x)
    */

  /**
   * convert a form which returns a validation of A into a form with returns A
   * ie lift the validation of A into the form validation
   */
  def validate[A,B](form: Form[Validation[B,A]], convertFailure: B => FormError): Form[A] = 
    Form(
      (env: Env) =>
	for {
	  frslt <- form.value(env) 
	  s <- init[FormState]
	} yield {
	  val valid: ValidForm[A] = frslt._1 match {
	    case Success(x) => x match {
	      case Success(xx) => success(xx)
	      case Failure(xy) => {
		val convertedFailure = xy match {
		  case e: Exception => e
		  case _            => GenericError((_: String) => xy.toString)
		}
		failure[(String,FormError),A](s._2.headOption.getOrElse("unknown"), convertFailure(xy)).liftFailNel
	      }}
	    case Failure(y) => failure(y)
	  }
	  (valid, frslt._2)
	}
    )

							   /*
  def validate[A](form: Form[Validation[Any,A]]): Form[A] = 
    Form(
      (env: Env) =>
	for {
	  frslt <- form.value(env) 
	  s <- init[FormState]
	} yield {
	  val valid: ValidForm[A] = frslt._1 match {
	    case Success(x) => x match {
	      case Success(xx) => success[NonEmptyList[(String,Exception)],A](xx)
	      case Failure(xy) => {
		val convertedFailure = xy match {
		  case e: Exception => e
		  case _            => GenericError(xy.toString)
		}		
		failure[NonEmptyList[(String,Exception)],A](
		  NonEmptyList( (s._2.headOption.getOrElse("unknown"), convertedFailure ) ))
	      }}
	    case Failure(y) => failure[NonEmptyList[(String,Exception)],A](y)
	  }
	  (valid, frslt._2)
	}
    )*/

  //def errorToString(i: String, e: Exception) =
    
  
  def runFormState[A](frm: Form[A], env: Env, showErrors: Boolean = true) = {
    val (valid,view) = (frm.value(env)) ! (0,List[String]())
    val errors: Errors = valid match {
      case Success(_) => Map()
      case Failure(x) => 
	if (showErrors) 
	  x.list.map((x: (String,FormError)) => (x._1, x._2.getErrorMessage(x._1))).toMap 
	else 
	  Map()
    }
    (valid, view(errors))
  }

  def getFormView[A](frm: Form[A]) =
    runFormState(frm, Map(), false)._2

  
  def getFormValidation[A](frm: Form[A], env: Env): Validation[NodeSeq,A] = {
    val (valid, view) = runFormState(frm, env) 
    valid match {
      case Success(a) => success[NodeSeq,A](a)
      case Failure(x) => failure[NodeSeq,A](view)
    }
  }

	 	 
}

