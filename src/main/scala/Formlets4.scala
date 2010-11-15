import scalaz._

/**
 * this sort of works but it's clumsy
 * also don't get html for partial forms
 */

object Formlets4 {
  import Scalaz._
  import scala.xml.NodeSeq
  import scala.xml.Text
  import Applicative._
  import Pointed._

  type Env = Map[String, String] // worry about files later
  type FormState = List[Int]

  /*
   * A view represents a visual representation of a form. It is a
   * function which takes a list of all errors and then produces a new view
   */
  type View = Map[String,String] => NodeSeq

  case class Form[A](
    fn: Env => Validation[NonEmptyList[String],A],
    view: Map[String,String] => NodeSeq
  )
    
  /*
  trait Form[A] = extends NewType[ Env => (Validation[Map[String,String],A],View) ]
  object Form {
    def apply[A](fn: Env => State[NodeSeq,ValidForm[A]]) = 
      new Form[A]{ val value = fn }
  }*/
  
  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) =
      Form((env: Env) => success(a), (errors: Map[String,String]) => Text(""))
  }

  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] = 
      Form(
	(env: Env) => r.fn(env) match {
	  case Success(a) => success[NonEmptyList[String],B](f(a))
	  case Failure(e) => failure[NonEmptyList[String],B](e)
	},
	r.view)
  }

  implicit def FormApply: Apply[Form] = new Apply[Form] {
    def apply[A,B](f: => Form[A => B], a: => Form[A]): Form[B] = 
      Form(
	(env: Env) => (f.fn(env),a.fn(env)) match {
	  case (Success(x),Success(y)) => success(x(y))
	  case (Success(x),Failure(y)) => failure[NonEmptyList[String],B](y)
	  case (Failure(x),Success(y)) => failure[NonEmptyList[String],B](x)
	  case (Failure(x),Failure(y)) => failure[NonEmptyList[String],B](x ⊹ y)
	},
	f.view ⊹ a.view)
  }

//  implicit def FormPointed[A] = pointed(FormFunctor, FormPure)

//  implicit def FormApplicative = applicative(FormPure, FormPointed)

  //implicit def FormPointed[A]: Pointed[PartialApply1Of2[Form, A]#Apply] = pointed[PartialApply1Of2[Form, A]#Apply](FormFunctor, FormPure)

  /*
 implicit def FormApplicative(implicit p: Pure[Form], a: Apply[Form], ): Applicative[Form] = new Applicative[Form] {
   def pure[A](a: => A) = p.pure(a)
   def apply[A, B](f: => Form[A => B], x: => Form[A]) = a(f, x)
 }*/

 def input(name: String): Form[String] =
   Form(
     (env: Env) =>  
       env.get(name).toSuccess[NonEmptyList[String]](nel("could not lookup for " + name,List())), 
     (errors: Map[String,String]) => 
	<input type="text" name={ name } id={ name } 
	value={ name } class="digestive-input" />)

  sealed trait Name extends NewType[String]
  object Name {
    def apply(s: String): Validation[String, Name] = 
      if (s.headOption.exists(_.isUpper))
	(new Name {val value = s}).success
      else
	"Name must start with a capital letter".fail
  }

  case class FullName(first: Name, second: Name)

  case class FullName2(first: String, second: String)

/*  def test(env: Map[String,String]) =
    input("first") 
*/


 // implicit def FormApplicative = applicative[PartialApply1Of2[Form]#Apply](FormPure, FormApply)

 
  //myForm.fn(Map("first"->"Jim", "last" -> "Bob"))

}

object Formlets4Test {
  import Scalaz._
  import Formlets4._

  val myForm = (input("first") ⊛ input("last")){FullName2(_,_)}

  def main(args: Array[String]) = {
    println(myForm.fn(Map("first"->"Jim")))
    println(myForm.view(Map("first"->"Jim")))
    println(myForm.fn(Map("first"->"Jim", "last" -> "Bob")))
    println(myForm.view(Map("first"->"Jim", "last" -> "Bob")))
  }

}
