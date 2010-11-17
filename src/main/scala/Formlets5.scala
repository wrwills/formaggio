package scormlets

import scalaz._

/**
 * trying again with state
 */

object Formlets5 {

  import Scalaz._
  import FromString._

  import scala.xml.NodeSeq
  import scala.xml.Text
  import Applicative._
  import Pointed._

  type Errors = Map[String,String]
  type Env = Map[String, String] // worry about files later
  type ValidForm[A] = Validation[NonEmptyList[(String,String)],A]

  /*
   * A view represents a visual representation of a form. It is a
   * function which takes a list of all errors and then produces a new view
   */
  type View = Errors => NodeSeq

  trait Form[A] extends NewType[ Env => State[Int,(ValidForm[A],View)] ] {

    /*
     * Append a unit form to the left. This is useful for adding labels or error
     * fields
     * only works if current form returns Unit
     */
    def ++>[B](frm: Form[B]): Form[B] = 
      Form((env: Env) => 
	for {s <- init[Int]
	     arslt <- this.value(env)
	     frslt <- frm.value(env)
	   } yield 
	     (frslt._1, arslt._2 ⊹ frslt._2))

    /*
     * Append a unit form to the right. See '++>'.
     */
    //def <++(frm: Form[Unit]): Form[A] = 
//   // <++ :: (Monad m, Monoid v)

  }
  object Form {
    def apply[A](fn: Env => State[Int,(ValidForm[A],View)]) = 
      new Form[A]{ val value = fn }
  }

  /*
  def liftValidationToForm[String => A](v: String => Validation[String,A]): Form[String => A] = {
    val newf = (s:String) => v(s) match {
      case Success(s) => success[NonEmptyList[(String,String)],A](s)
      case Failure(e) => failure[NonEmptyList[(String,String)],A](NonEmptyList("",e))
    }
    Form((env: Env) => 
      for {s <- init[Int]} 
      yield (v(
      */


  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) =
      Form((env: Env) => 
	for {s <- init[Int]} 
	yield (success(a), (errors: Map[String,String]) => Text("")))
  }

  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] = 
      Form((env: Env) => 
	for {s <- init[Int];
//	     _ <- modify((_: Int) + 1);
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
	for {s <- init[Int];
	     frslt <- f.value(env); 
	     arslt <- a.value(env)
	   } yield {
	     println("applying: state is " + s)
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
     (env: Env) =>  
       	for {s <- init[Int] 
	     _ <- modify((_: Int) + 1)
	   } yield {
	  val lookupName = name + s
	  println("lookupName " + lookupName)
	  val valid =
            env.get(lookupName).toSuccess[NonEmptyList[(String,String)]](
	      nel((lookupName, "could not lookup for " + name),List()))
	  val view =
	    (errors: Map[String,String]) => 
	      <input type="text" name={ lookupName } id={ lookupName } value={ errors.get(lookupName).toString } class="digestive-input" />
	  (valid,view)
	})

  // need to modify state monad to be (String,Int) for this to work
  def label(name: String): Form[Unit] = 
   Form(
     (env: Env) =>  
       for {s <- init[Int]} yield 
	 {
	     val lab = 
	       (errors: Map[String,String]) => <label for={ "name" + s } class="digestive-label">{ name }</label>
	     ( success(()),  lab )
	 }
   )


    

  /*
 def inputN(name: String): Form[Name] =
   Form(
     (env: Env) =>  
       	for {s <- init[Int] 
	     _ <- modify((_: Int) + 1)
	   } yield {
	  val lookupName = name + s
	  println("lookupName " + lookupName)
	  val valid: Validation[String,Name] =
	    for (a <- env.get(lookupName).toSuccess[String]("could not lookup for " + lookupName);
		 b <- Name(a)) yield b
	  val view =
	    (errors: Map[String,String]) => 
	      <input type="text" name={ lookupName } id={ lookupName } value={ errors.get(lookupName).toString } class="digestive-input" />
	  (valid ,view)
	})*/


  /*
 def inputText[A](name: String = "")(implicit f: FromString[A]): Form[A] =
   Form(
     (env: Env) =>  
       	for {s <- init[Int] 
	     _ <- modify((_: Int) + 1)
	   } yield {
	     val lookupName = name + s
	     println("lookupName " + lookupName)
	     val valid =
               env.get(lookupName).toSuccess[NonEmptyList[(String,String)]](
		 nel((lookupName, "could not lookup for " + name),List())).fromString
	     val view =
	       (errors: Map[String,String]) => 
		 <input type="text" name={ lookupName } id={ lookupName } value={ errors.get(lookupName).toString } class="digestive-input" />
	     (valid,view)
	   }
   )
   */

  //def inputAndLabel(name: String): Form[String] =
    


  def runFormState[A](frm: Form[A], env: Env, showErrors: Boolean = true) = {
    val (valid,view) = (frm.value(env)) ! 0
    val errors: Errors = valid match {
      case Success(_) => Map()
      case Failure(x) => if (showErrors) x.list.toMap else Map()
    }
    (valid, view(errors))
  }

  def getFormView[A](frm: Form[A]) =
    runFormState(frm, Map(), false)._2
  

  def validate[A](form: Form[Validation[String,A]]): Form[A] = 
    Form(
      (env: Env) =>
	for {
	  s <- init[Int];
	  frslt <- form.value(env) 
	} yield {
	  val valid = frslt._1 match {
	    case Success(x) => x match {
	      case Success(xx) => success[NonEmptyList[(String,String)],A](xx)
	      case Failure(xy) => failure[NonEmptyList[(String,String)],A](NonEmptyList( ("",xy) ))
	    }
	    case Failure(y) => failure[NonEmptyList[(String,String)],A](y)
	  }
	  (valid, frslt._2)
	}
    )
	 	 
}

object Formlets5Test {
  import Scalaz._
  import Formlets5._

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

}

