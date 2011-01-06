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

case object EmptyStringError extends FormError {
  override def getErrorMessage(i: String) = "empty string not allowed for " + i
}

case class GenericError(msg: String => String) extends FormError {
  override def getErrorMessage(i: String) = msg(i)
}

trait Formlets

/**
 * Formlets
 */
object Formlets extends Html with MassInput {

  import Scalaz._

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

  def addIndexToValidation[A](i: String, v: Validation[FormError,A]): Validation[(String,FormError),A] =
    v match {
      case Success(s) => s.success
      case Failure(f) => failure(i, f)
    }     
  
  def liftExceptionValidation[A](v: Validation[Exception,A]): Validation[(String,Exception),A] =
    v match {
      case Success(s) => s.success[(String,Exception)]
      case Failure(f) => failure[(String,Exception),A]("", f)
    } 

  type Errors = Map[String,String]
  type Env = Map[String, String] // worry about files later
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

  // transform the xml component
    def plug(transform: NodeSeq => NodeSeq): Form[A] =
      Form((env: Env) => 
	for {
	  s <- init[FormState]
	  arslt <- this.value(env)
	} yield 
	  (arslt._1, 
	   (err: Errors) => transform( arslt._2(err) ) ) )
	  
	

      

  }
      

  object Form {
    def apply[A](fn: Env => State[FormState,(ValidForm[A],View)]) = 
      new Form[A]{ val value = fn }
  }

  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) =
      Form((env: Env) => 
	for {s <- init[FormState]} 
	yield (a.success, (errors: Map[String,String]) => Text("")))
  }
  
  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] = 
      Form((env: Env) => 
	for { rslt <-r.value(env) } 
	yield (rslt._1 map f, rslt._2))
  }

  implicit def FormApply: Apply[Form] = new Apply[Form] {
    def apply[A,B](f: => Form[A => B], a: => Form[A]): Form[B] = 
      Form(
	(env: Env) =>
	  for {
	    frslt <- f.value(env)
	    arslt <- a.value(env)
	  } yield (arslt._1 <*> frslt._1, frslt._2 ⊹ arslt._2))
  }

  /*
  def massInput[A](frm: Form[A]): Form[Seq[A]] = 
    Form(
      (env: Env) =>
        for {
          frslt <- form.value(env) 
          s <- init[FormState]
        } yield (
	  
	)
    )*/


  def validate[A](form: Form[Validation[String,A]]): Form[A] = 
    validate(form, (x:String) => GenericError((_: String) => x) )
  
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
        } yield (
	  frslt._1.fold(
	    _.fail[A],
	    _.fail.map( 
	      f => (s._2.headOption.getOrElse("unknown"), convertFailure(f)) ).validation.liftFailNel ),
	  frslt._2)
    )

  def convertErrorToErrorMessage(x: (String,FormError)) = (x._1, x._2.getErrorMessage(x._1))
     
  /**
   * run a form within an environment
   */
  def runFormState[A](frm: Form[A], env: Env, showErrors: Boolean = true) = {
    val (valid,view) = frm(env) ! (0,List[String]())
    //val (valid,view) = frm(env) ! (1,List[String]())
    val errors: Errors = 
      if (showErrors)
	valid.fail.map(
	  _.list.map( convertErrorToErrorMessage(_) ).toMap
	).toOption.getOrElse(Map())
      else
	Map()
    (valid, view(errors))
  }

  /**
   * get the view the form without any errors displayed
   */
  def getFormView[A](frm: Form[A]) =
    runFormState(frm, Map(), false)._2

  /**
   * return a validation showing either the xml for the view or the result
   */
  def getFormValidation[A](frm: Form[A], env: Env): Validation[NodeSeq,A] = {
    val (valid, view) = runFormState(frm, env) 
    valid.fail.map(_ => view).validation
  }

  /**
   * like getFormValidation but returns an Either for callers who do not
   * want to import scalaz to use  this library
   */
  def getFormEither[A](frm: Form[A], env: Env) = 
    getFormValidation(frm, env).either
	 	 
}

