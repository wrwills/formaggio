package scormlets

import scalaz._
import org.specs._

import Formlets._
import Html._

class FormletsSpecs extends Specification {
  import Scalaz._

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

  "passing correct parameters should produce a result" in {
    runFormState( labelledFullNameForm, Map("name0"-> "Jim", "name1" -> "Bob")) must_== 
      FullName(Name("Jim").toOption.get,Name("Bob").toOption.get)
  }

}


