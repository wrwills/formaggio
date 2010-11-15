import scalaz._

/**
 * trying again with state
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
	for {s <- init[Int];
	     _ <- modify((_: Int) + 1);
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

//      error("undefined")
/*
 * 	     (env: Env) => (failure(NonEmptyList(("aa","bb"))),(errors: Errors) => Text(""))
 *       Form(
	for {s <- init[Int];
	     _ <- modify((_: Int) + 1);
	     rslt <-r.value
	   } yield
	     (env: Env) =>
	       (failure(NonEmptyList(("aa","bb"))),(errors: Errors) => Text(""))
      }

	  ((env: Env) =>
	    s.value(env) match {
	      case (Success(a),view) => (success(f(a)),view)
		  case (Failure(e),view) => (failure(e),view)
	    })
  */ 


}
