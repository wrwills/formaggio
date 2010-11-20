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

  val myNameForm: Form[Name] = validate(input("name") ∘ Name.apply )
  val fullNameForm = (myNameForm  ⊛ myNameForm){  FullName(_,_) }

  def labelledNameForm(s: String) =  validate(label(s) ++> (input("name") ∘ Name.apply))
  //def labelledNameForm(s: String) =  validate(label(s) ++> (input("name") ∘ Name.apply)) <++ ferrors

  val labelledFullNameForm = (labelledNameForm("First")  ⊛ labelledNameForm("Second")){ FullName(_,_) } <++ ferrors



//    (view \\ "input") filter ( (x:Node) => (x \ "@id").text == s ) map ( (_:Node) \ "@value") head
  def getInput(s: String, view: NodeSeq) =  
    ((view \\ "input") filter ((x:Node) => (x \ "@id").text == s)).head
  /*
    (for {i <- view \\ "input";
	  if( (i \ "@id").text == s) }
     yield i).head*/


  def getValueForInput(s: String, view: NodeSeq) =  
    (getInput(s,view) \ "@value").text
/*
    (for {i <- view \\ "input";
	  if( (i \ "@id").text == s) }
     yield (i \ "@value").toString).head*/
     

  def getValueForLabel(s: String, view: NodeSeq) =
    (view \\ "label") filter ( _.text == s ) map (_ \ "@for") map (_.text) head
  /*
    (for {i <- view \\ "label"
	  if (i.text == s) }
     yield (i \ "@for").text).head
     */

  def getErrorMessageForField(s: String, view: NodeSeq) =  
    (for {i <- view \\ "li";
	  if( (i \ "@id").text == s) }
     yield i.text).head

  
  "form view should be correct" in {

    val view = runFormState(labelledFullNameForm, Map("name1"-> "Jim", "name2" -> "Bob"))._2
    println(view)

    "fields should be filled in with environment values" in {
      getValueForInput("name1", view) must_== "Jim"
      getValueForInput("name2", view) must_== "Bob"
    }

    "labels should be filled in correctly" in {
      getValueForLabel("First", view) must_== "name1"
      getValueForLabel("Second", view) must_== "name2"
    }

  }

  "form results should be correct" in {
    "passing correct parameters should produce a result" in {
      getFormValidation( labelledFullNameForm, Map("name1"-> "Jim", "name2" -> "Bob")).toOption.get.toString must_== 
      FullName(Name("Jim").toOption.get,Name("Bob").toOption.get).toString
    }

    "form should fail if one of the values isn't filled in" in {
      val rslt = runFormState(labelledFullNameForm, Map("name1"-> "Jim"))
      println(rslt._2)
      
      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name2","could not lookup for name")
    }

    "form should fail if one of the values isn't filled in correctly" in {
      val rslt = runFormState(labelledFullNameForm, Map("name1"-> "Jim", "name2" -> "bob"))
      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name2","Name must start with a capital letter")

      val view = rslt._2
      println(view)
      getValueForInput("name1", view) must_== "Jim"
      getValueForInput("name2", view) must_== "bob"
      

      "and the view should contain an appropriate error message" in {
	getErrorMessageForField("name2", view) must_== "Name must start with a capital letter"
	(getInput("name2", view) \ "@class").text must_== "digestive-input-error"
	//for {
      }
    }
  }

  

}


