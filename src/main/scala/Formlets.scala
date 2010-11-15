import scalaz._

import java.io.File
//import scala.xml._


/**
 * formlets are composable: combine lots of them to produce a form
 * so a single input has the same time as a form with many inputs
 */

/*
 * http://groups.inf.ed.ac.uk/links/formlets/
 * 
 * implementations
 * racket: http://docs.racket-lang.org/web-server/formlets.html
 */



/*
object ValidForm {
  def apply(s: 
}*/

case class Address(addressLine: String, addressCity: String, addressPostal: Int)


/*
module type FORMLET = sig
include Idiom
val xml : xml → unit t
val text : string → unit t
val tag : tag → attrs → α t → α t
val input : string t
val run : α t → xml × (env → α)
end
*/

//trait 
//newtype Form xml m a = Form { deform :: Env -> S (m (Validator a), m xml, FormContentType) }

/*
-- | An input field with an optional value
input :: Monad m => XHtmlFormlet m String
input = input' (\n v -> X.textfield n ! [X.value v])
*/
//-- | Helper function for genereting input components based forms.
//input' :: Monad m => (String -> String -> xml) -> Maybe String -> Form xml m String
//input' i = inputM' (\n -> return . i n)
//def input(inputF: (String, String) => Xml, Option[String]): Form

//inputM' :: Monad m => (String -> String -> m xml) -> Maybe String -> Form xml m String
//def inputM[M](f: (String,String) => M[Xml], Option[String]): Form = 

object Formlets {
  import Scalaz._

  type Env = Map[String, String] // worry about files later
  type FormState = List[Int]
  type S[A] = State[FormState,A]
  //type Validator a = S (FR.FormResult a)

  type Name = String


  // error will be a list of errors and xml; success is the data type
  //type ValidForm[A] = Validation[(NonEmptyList[String],scala.xml.Node),A]
  type ValidForm[A] = Validation[NonEmptyList[String],A]

  //newtype Form xml m a = Form { deform :: Env -> S (m (Validator a), m xml, FormContentType) }
  // drop xml abstraction for now
  //class Form[A] extends NewType[ Env => State[Validator, Xml]]
  //class Form[A] extends NewType[ Env => State[ValidForm[A]]]
  //type Form[A] =  NewType[ Env => ValidForm[A]]
  /*
  trait Form[A] extends NewType[ Env => Validation[(NonEmptyList[String],scala.xml.Node),A]]
  object Form {
    def apply[A](fn: Env => Validation[(NonEmptyList[String],scala.xml.Node),A]) = 
      new Form[A]{ val value = fn }
  }*/
  /*
  trait Form[A] extends NewType[ Env => Validation[NonEmptyList[String],A]]
  object Form {
    def apply[A](fn: Env => Validation[NonEmptyList[String],A]) = 
      new Form[A]{ val value = fn }
  }*/
  
  trait Form[A] extends NewType[ Env => ValidForm[A]]
  object Form {
    def apply[A](fn: Env => ValidForm[A]) = 
      new Form[A]{ val value = fn }
  }


  //type Formlet xml m a = Maybe a -> Form xml m a
  type Formlet[A] = Option[A] => Form[A]


  /*
  def input(name: String): Formlet[String] = {
    val lookupName = name // later use statemonad to wrap this up    
    val form = (env: Env) =>  
      success[NonEmptyList[String],Node]Text(name + ":") *> (env.get(lookupName)  ∘ Date.curried)
    (inp: Option[String]) => form
  }*/

  def input[A](name: String): Form[Date] = {
  //def input(name: String) = {
    val lookupName = name // later use statemonad to wrap this up    
    val form = (env: Env) =>  
      success[NonEmptyList[String],String]("") *> 
      (env.get(lookupName).toSuccess(
	nel("could not lookup for " + name,List())
      )  ∘ Date)
    Form(form)
  }

  def input2[A](name: String, fn: String => A): Form[A] = {
  //def input(name: String) = {
    val lookupName = name // later use statemonad to wrap this up    
    val form = (env: Env) =>  
      success[NonEmptyList[String],String]("") *> 
      (env.get(lookupName).toSuccess(
	nel("could not lookup for " + name,List())
      )  ∘ fn)
    Form(form)
  }

  /*
  def input(name: String): Form[Date] = {
  //def input(name: String) = {
    val lookupName = name // later use statemonad to wrap this up    
    val form = (env: Env) =>  
      success[NonEmptyList[String],scala.xml.Node](scala.xml.Text(name + ":")) *> 
      (env.get(lookupName).toSuccess(
	nel("could not lookup for " + name,List()),
	<input type="text" name="some-form-f0" id="some-form-f0" value="" class="digestive-input" />)
      )  ∘ Date)
    Form(form)
  }*/


  /*
  def inputText
  
  val addressForm1: Form[Address]  =
    (inputText(None) ⊛ inputText(Some("Ghent")) ⊛ inputTextRead("No read", Some(9000))){Address(_,_,_)}
    */
 /* 
  def dateformlet: Formlet[Date]  = date formlet = formlet
<div>
Month: {input int ⇒ month}
Day: {input int ⇒ day}
</div>

}
*/
  case class Date(s: String)

  
  
  //def input(name: String): State[List[String], Formlet[Date]] = {
  /*
  def input(name: String): State[List[String], Formlet[Date]] = {
    val lookupName = name // later use statemonad to wrap this up
    (env: Env) => Text(name + ":") *> (env.get(lookupName)  ∘ Date.curried)
  }*/


}


object Persons {
  import Scalaz._

  sealed trait Name extends NewType[String]
  object Name {
    def apply(s: String): Validation[String, Name] = 
      if (s.headOption.exists(_.isUpper))
	(new Name {val value = s}).success
      else
	"Name must start with a capital letter".fail
  }

  sealed trait Age extends NewType[Int]
  object Age {
    def apply(a: Int): Validation[String, Age] = 
      if (0 to 130 contains a)
	(new Age {val value = a}).success
      else
	"Age must be in range".fail
  }

  case class Person(name: Name, age: Age)

  def mkPerson(name: String, age: Int) = (Name(name).liftFailNel ⊛ Age(age).liftFailNel){ (n, a) => Person(n, a)}

  def mkPerson2(name: String, age: Int) = (Name(name).liftFailNel ⊛ Age(age).liftFailNel){ Person(_,_) }

  def mkPerson3(name: String, age: Int) = Age(age).liftFailNel <*> (Name(name).liftFailNel ∘ Person.curried)

  def mkPerson4(name: String, age: Int): Validation[NonEmptyList[String],Person] = 
    success[NonEmptyList[String],String]("New") *> 
    (Age(age).liftFailNel <*> (Name(name).liftFailNel ∘ Person.curried))

  
  def mkAge(env: Map[String,String]): Validation[NonEmptyList[String],Age] = env.get("age") match {
    case Some(age) => 
      age.parseInt.liftFailNel match {
	case Success(a) => Age(a).liftFailNel
	case Failure(e) => failure[NonEmptyList[String],Age](e.map(_.toString))
      }      
    case None      => failure[NonEmptyList[String],Age](NonEmptyList("need age"))
  }

  def mkName(env: Map[String,String]): Validation[NonEmptyList[String],Name] = env.get("name") match {
    case Some(age) => Name(age).liftFailNel
    case None      => failure[NonEmptyList[String],Name](NonEmptyList("need name"))
  }

  def mkPerson6(env: Map[String,String]) = (mkName(env) ⊛ mkAge(env))

  def mkPerson5(env: Map[String,String]): Validation[NonEmptyList[String],Person] = 
    (mkName(env) ⊛ mkAge(env)){Person(_,_)}

  def mkNamedField(name: String): Map[String,String] => State[Int,Validation[NonEmptyList[String],Name]] = 
    (env: Map[String,String]) =>
      env.get("name") match {
	case Some(age) => for ( s <- init[Int] ) yield Name(age).liftFailNel
	case None      => {
	  (init[Int] <* modify((_: Int) + 1)) ∘ {
	    (s: Int) => 
	      failure[NonEmptyList[String],Name](NonEmptyList("need name"))
	  }
	}      
      }

  //type Formlet[A] = ReaderT[State[Int,Validation[NonEmptyList[String],A]], Map[String,String], A]
  //trait  Formlet[A] extends ReaderT[List[_], Map[String,String], A]

  case class FullName(first: Name, second: Name)

  /*
  mkNamedField("first") ⊛ mkNamedField("second") 

  def mkFullName(env: Map[String,String]): State[Int,Validation[NonEmptyList[String],FullName]] = 
    ( (mkNamedField("first"))(env) ⊛ (mkNamedField("second"))(env) ){FullName(_,_)}
    */

  /*
  def mkNamedField2(name: String): State[Map[String,String],Validation[NonEmptyList[String],Name]] = 
    for {env <- init[Map[String,String]]} 
    yield 
      env.get(name) match {
	case Some(age) => Name(age).liftFailNel
	case None      => failure[NonEmptyList[String],Name](NonEmptyList("need name"))	  
      }      

  def mkFullName2 = ( mkNamedField2("first") ⊛ mkNamedField2("second") ){FullName(_,_)}
  */
 // val test = println(mkNamedField2("first") ! 


}

