package scormlets

import scalaz._

/**
 * trying again with state
 */

object Formlets {

  import Scalaz._
  import FromString._

  import scala.xml.NodeSeq
  import scala.xml.Text
  import Applicative._
  import Pointed._

  type Errors = Map[String,String]
  type Env = Map[String, String] // worry about files later
  type ValidForm[A] = Validation[NonEmptyList[(String,String)],A]

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
     */    
    def ++>[B](frm: Form[B]): Form[B] = 
      Form((env: Env) => 
	for {
	     frslt <- frm.value(env)
	     arslt <- this.value(env)
	     s <- init[FormState]
	   } yield
	     (frslt._1, arslt._2 ⊹ frslt._2))
	 
  }
    /*
     * Append a unit form to the right. See '++>'.
     */
    /*
    def <++(frm: Form[Unit]): Form[A] = 
      Form((env: Env) => 
	for {s <- init[FormState]
	     arslt <- this.value(env)
	     frslt <- frm.value(env)
	   } yield 
	     (frslt._1, arslt._2 ⊹ frslt._2))
      */
//   // <++ :: (Monad m, Monoid v)



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
	     println("applying: state is " + s)
	     println("arslt: " + arslt)
	     println("frslt: " + arslt)
	     val valid = (frslt._1,arslt._1)  match {
	       case (Success(x),Success(y)) => success(x(y))
	       case (Success(x),Failure(y)) => failure(y)
	       case (Failure(x),Success(y)) => failure(x)
	       case (Failure(x),Failure(y)) => failure(x ⊹ y)
	     }
	     (valid, frslt._2 ⊹ arslt._2)
	   })
  }

  /**
   * convert a form which returns a validation of A into a form with returns A
   * ie lift the validation of A into the form validation
   */
  def validate[A](form: Form[Validation[String,A]]): Form[A] = 
    Form(
      (env: Env) =>
	for {
	  s <- init[FormState];
	  frslt <- form.value(env) 
	} yield {
	  val valid = frslt._1 match {
	    case Success(x) => x match {
	      case Success(xx) => success[NonEmptyList[(String,String)],A](xx)
	      case Failure(xy) => failure[NonEmptyList[(String,String)],A](
		NonEmptyList( (s._2.headOption.getOrElse("unknown"),xy) ))
	    }
	    case Failure(y) => failure[NonEmptyList[(String,String)],A](y)
	  }
	  (valid, frslt._2)
	}
    )
  
  def runFormState[A](frm: Form[A], env: Env, showErrors: Boolean = true) = {
    val (valid,view) = (frm.value(env)) ! (0,List[String]())
    //val (valid,view) = (frm.value(env)) ! 0
    val errors: Errors = valid match {
      case Success(_) => Map()
      case Failure(x) => if (showErrors) x.list.toMap else Map()
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
/*
object Formlets5Test {
  import Scalaz._
  import Formlets._
  import Html._

  case class FullName(first: Name, second: Name)
  case class FullName2(first: String, second: String)
  val myForm = (input("first") ⊛ input("last")){FullName2(_,_)}

  sealed trait Name extends NewType[String]
  object Name {
    def apply(s: String): Validation[String, Name] = 
      if (s.headOption.exists(_.isUpper))
	(new Name {val value = s}).success
      else
	"Name must start with a capital letter".fail
  }

  //val myNameForm: Form[Validation[String,Name]] = input("name") ∘ Name.apply 
  val myNameForm: Form[Name] = validate(input("name") ∘ Name.apply )
    val fullNameForm = (myNameForm  ⊛ myNameForm){  FullName(_,_) }
  //val myFormTwo = (myNameForm ⊛ myNameForm){ FullName(_,_) }

  // 
  //val labelledFullNameForm = (label("First") ⊛ myNameForm ⊛ label("Second") ⊛ myNameForm){  FullName(_,_) }
  def labelledNameForm(s: String) =  validate(label(s) *> (input("name") ∘ Name.apply))
  val labelledFullNameForm = (labelledNameForm("First")  ⊛ labelledNameForm("Second")){ FullName(_,_) }

  def main(args: Array[String]) = {
    /*
    println((myForm.value)(Map()) ! 0)
    println((myForm.value)(Map("first1"->"Jim")) ! 0)
    println((myForm.value)(Map("first1"->"Jim", "last2" -> "Bob")) ! 0)
    */
    println(runFormState(myForm, Map("first1"->"Jim")))
    println(runFormState(myForm, Map("first1"->"Jim", "last2" -> "Bob")))
    println(getFormView(myForm))	    

    println(runFormState( fullNameForm, Map("name2"-> "Jim", "name4" -> "Bob")))

    println(runFormState( labelledFullNameForm, Map("name0"-> "Jim", "name1" -> "Bob")))
  }

}*/

