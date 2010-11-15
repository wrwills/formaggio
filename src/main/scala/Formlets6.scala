import scalaz._

/**
 * putting the state monad on the outside
 * 
 */

object Formlets6 {

  import Scalaz._
  import scala.xml.NodeSeq
  import scala.xml.Text
  import Applicative._
  import Pointed._


  type Env = Map[String, String] // worry about files later
  type Errors = Map[String,String]
  type ValidForm[A] = Validation[NonEmptyList[(String,String)],A]

  /*
   * A view represents a visual representation of a form. It is a
   * function which takes a list of all errors and then produces a new view
   */
  type View = Map[String,String] => NodeSeq

  //case class FormInternal[A](
    

  trait Form[A] extends NewType[ State[Int, Env => (ValidForm[A],View)] ]
  object Form {
    def apply[A](fn: State[Int, Env => (ValidForm[A],View)]) = 
      new Form[A]{ val value = fn }
  }

  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) =
      Form(
	for {s <- init[Int]} yield
	  (env: Env) => (success(a), (errors: Errors) => Text("")))
  }

  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] =
      Form(
	for {s <- init[Int];
	     _ <- modify((_: Int) + 1);
	     rslt <-r.value
	   } yield  
	     (env: Env) => rslt(env)  match {
	       case (Success(a),view) => (success(f(a)),view)
	       case (Failure(e),view) => (failure(e),view)
	     }
	   
      )
  }

  implicit def FormApply: Apply[Form] = new Apply[Form] {
    def apply[A,B](f: => Form[A => B], a: => Form[A]): Form[B] = 
      Form(
	for {s <- init[Int]
	     fform <- f.value
	     aform <- a.value
	   } yield  
	     (env: Env) => {
	       val (frslt,arslt) = (fform(env),aform(env))
	       val valid = (frslt._1,arslt._1)  match {
		 case (Success(x),Success(y)) => success(x(y))
		 case (Success(x),Failure(y)) => failure(y)
		 case (Failure(x),Success(y)) => failure(x)
		 case (Failure(x),Failure(y)) => failure(x ⊹ y)
	       }
	       (valid, frslt._2 ⊹ arslt._2)
	     })
  }

  def input(name: String): Form[String] =
    Form(
      for {s <- init[Int];
	   _ <- modify((_: Int) + 1)
	 } yield (env: Env) => {
	  val lookupName = name + s 
	  val valid =
            env.get(lookupName).toSuccess[NonEmptyList[(String,String)]](
	      nel((lookupName, "could not lookup for " + name),List()))
	  val view =
	    (errors: Map[String,String]) => 
	      <input type="text" name={ lookupName } id={ lookupName } value={ env.get(lookupName).toString } class="digestive-input" />
	  (valid,view)
	})

  /*
  implicit def FormIndex: Index[Form] = new Index[Form] {
    def index[A](a: Form[A], i: Int) = None
  }*/

}

object Formlets6Test {
  import Scalaz._
  import Formlets6._

  case class FullName2(first: String, second: String)
  val myForm = (input("first") ⊛ input("last")){FullName2(_,_)}
  
  def main(args: Array[String]) = {
    println((myForm.value ! 0)(Map()))
    println(((myForm.value ! 0)(Map())._2)(Map())  )
    println((myForm.value ! 0)(Map("first1"->"Jim")))
    println((myForm.value ! 0)(Map("first1"->"Jim", "last2" -> "Bob")))
  }

}

