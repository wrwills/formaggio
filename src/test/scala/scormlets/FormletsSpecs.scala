package scormlets

import scalaz._
import org.specs._

import Formlets._
import Html._

class FormletsSpecs extends Specification {
  import Scalaz._
  import scala.xml._

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
  //def labelledNameForm(s: String) =  validate(label(s) *> (input("name") ∘ Name.apply))
  def labelledNameForm(s: String) =  validate(label(s) ++> (input("name") ∘ Name.apply))

  val labelledFullNameForm = (labelledNameForm("First")  ⊛ labelledNameForm("Second")){ FullName(_,_) }


  def getValueForInput(s: String, view: NodeSeq) =
    (for {i <- view \\ "input";
	  r <- i \ "@id";
	  if (r==Text(s)) }
     yield (i \ "@value").toString).head

  /*
  def getValueForLabel(s: String, view: NodeSeq) =
    for {i <- view \\ "label";
	  r <- i \ "@for";
	  if (r==Text(s)) }
     yield i.text*/

  def getValueForLabel(s: String, view: NodeSeq) =
    for {i <- view \\ "label";
	  if (i.text == s) }
     yield (i \ "@for").text

  
  "form view should be correct" in {

    val view = runFormState(labelledFullNameForm, Map("name0"-> "Jim", "name1" -> "Bob"))._2
    println(view)

    "fields should be filled in with environment values" in {
      getValueForInput("name0", view) must_== "Jim"
      getValueForInput("name1", view) must_== "Bob"
    }

    "labels should be filled in correctly" in {
      getValueForLabel("First", view) must_== "name0"
      getValueForLabel("Second", view) must_== "name1"
    }

  }

  "form results should be correct" in {
    "passing correct parameters should produce a result" in {
      getFormValidation( labelledFullNameForm, Map("name0"-> "Jim", "name1" -> "Bob")).toOption.get.toString must_== 
      FullName(Name("Jim").toOption.get,Name("Bob").toOption.get).toString
    }

    "form should fail if one of the values isn't filled in" in {
      val rslt = runFormState(labelledFullNameForm, Map("name0"-> "Jim"))
      
      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name1","could not lookup for name")
    }

    "form should fail if one of the values isn't filled in correctly" in {
      val rslt = runFormState(labelledFullNameForm, Map("name0"-> "Jim", "name1" -> "bob"))
      rslt._1.isFailure must beTrue
      //rslt._1.fail.toOption.get.head must_==  ("","Name must start with a capital letter")
      // TODO: make index for validation failures
      rslt._1.fail.toOption.get.head must_==  ("name1","Name must start with a capital letter")

      val view = rslt._2
      getValueForInput("name0", view) must_== "Jim"
      getValueForInput("name1", view) must_== "bob"
    }
  }

  

}

