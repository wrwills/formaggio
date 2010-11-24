package scormlets

import scalaz._


/**
 * keep these in core so they can be reused across sample apps and testing
 */
object SampleData {
  import Scalaz._
  import Formlets._
  import Html._
  

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

  val myForm = (inputText("first") ⊛ inputText("last")){FullName2(_,_)}
  val myNameForm: Form[Name] = validate(inputText("name") ∘ Name.apply )
  val fullNameForm = (myNameForm  ⊛ myNameForm){  FullName(_,_) }

  def labelledNameForm(s: String) =  validate(label(s) ++> (inputText("name", Some("Billy")) ∘ Name.apply))

  val labelledFullNameForm = (labelledNameForm("First")  ⊛ labelledNameForm("Second")){ FullName(_,_) } <++ ferrors

  sealed trait Age extends NewType[Int]
  object Age {
    def apply(a: Int): Validation[String, Age] = 
      if (0 to 130 contains a)
	(new Age {val value = a}).success
      else
	"Age must be in range 0 to 130".fail

    // TODO: figure out a way to make this Validation conversion implicit?
    def apply(a: String): Validation[String, Age] = 
      a.parseInt match {
	case Success(s) => apply(s)
	case Failure(e) => failure[String, Age](e.toString)
      }

  }

  val ageForm = validate(label("Age") ++> (inputText("age") ∘ Age.apply))

  // TODO: check that can handle optional values without having entire form fail
  case class Person(name: FullName, age: Age, married: Boolean, nickname: Option[String], password: String)

  def passwordVerify(x: String)(y: String): Validation[String, String] = 
    if (x == y) x.success[String] else failure[String,String]("passwords don't match")

  // TODO: numbering for this is coming out backwards 
  // doesn't matter for now but need to look at ordering of fields using different notations  
  val passwordValidation = (label("Password") ++> (inputPassword("password") <*> 
					     (label("Password (verify)") ++> (inputPassword("password"))
					     ∘ passwordVerify ) ) )

  val personForm = (labelledFullNameForm ⊛ 
		    ageForm ⊛ 
		    ((inputCheckbox("married")) <++ ferrors) ⊛ 
		    optionalInputText("nickname") ⊛
		    (validate(passwordValidation) <++ ferrors)
		  ){Person(_,_,_,_,_)}

}
