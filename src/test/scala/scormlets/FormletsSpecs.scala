package scormlets

import scalaz._
import org.specs._

import Formlets._
import Html._


object ViewHelpers {
  import scala.xml._
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
     yield i.text).headOption

  
}

object FormletsSpecs extends Specification {  
  import Scalaz._
  import ViewHelpers._

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

  def labelledNameForm(s: String) =  validate(label(s) ++> (inputText("name") ∘ Name.apply))

  val labelledFullNameForm = (labelledNameForm("First")  ⊛ labelledNameForm("Second")){ FullName(_,_) } <++ ferrors

  sealed trait Age extends NewType[Int]
  object Age {
    def apply(a: Int): Validation[String, Age] = 
      if (0 to 130 contains a)
	(new Age {val value = a}).success
      else
	"Age must be in range".fail

    // TODO: figure out a way to make this Validation conversion implicit?
    def apply(a: String): Validation[String, Age] = 
      a.parseInt match {
	case Success(s) => apply(s)
	case Failure(e) => failure[String, Age](e.toString)
      }

  }

  //val ageForm = validate(label("Age") ++> (inputText("age") ∘ (_: String).parseInt))
  val ageForm = validate(label("Age") ++> (inputText("age") ∘ Age.apply))
  //case class Person(name: FullName, age: Age)

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
      rslt._1.fail.toOption.get.head must_==  ("name2","name can not be empty")
    }

    "form should fail if one of the values isn't filled in correctly" in {
      val rslt = runFormState(labelledFullNameForm, 
			      Map("name1"-> "Jim", 
				  "name2" -> "bob"
				))
      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name2","Name must start with a capital letter")

      val view = rslt._2
      println(view)
      getValueForInput("name1", view) must_== "Jim"
      getValueForInput("name2", view) must_== "bob"
      

      "and the view should contain an appropriate error message" in {
	getErrorMessageForField("name2", view) must beSome("Name must start with a capital letter")
	(getInput("name2", view) \ "@class").text must_== "digestive-input-error"
	//for {
      }
    }
  }

  "for a person form" in {
    val form = personForm

    val env = 
      Map(
	"name1"-> "Jim", 
	"name2" -> "Bob", 
	"age3" -> "30",
	"password7" -> "password",
	"password6" -> "password"				     
      )


    val successRslt = runFormState(form, env)
    println(successRslt)

    "form view should be correct" in {
      val view = successRslt._2

      "fields should be filled in with environment values" in {
	getValueForInput("name1", view) must_== "Jim"
	getValueForInput("name2", view) must_== "Bob"
	getValueForInput("age3", view) must_== "30"
      }

      "labels should be filled in correctly" in {
	getValueForLabel("First", view) must_== "name1"
	getValueForLabel("Second", view) must_== "name2"
	getValueForLabel("Age", view) must_== "age3"
      }

    }
    
    "form should fail if one of the values isn't filled in" in {
      val rslt = runFormState(form, Map("name1"-> "Jim"))
      println(rslt)
      
      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name2","name can not be empty")
    }

    "form should fail if one of the values isn't filled in correctly" in {
      val rslt = runFormState(form, env + ("name2" -> "bob") + ("password6" -> "passwd"))
      println(rslt)
      rslt._1.isFailure must beTrue

      val errors = rslt._1.fail.toOption.get.list.toMap
      errors.get("name2") must_== Some("Name must start with a capital letter")
      errors.get("password7") must_== Some("passwords don't match")

      val view = rslt._2
      getValueForInput("name1", view) must_== "Jim"
      getValueForInput("name2", view) must_== "bob"
      
      "and the view should contain an appropriate error message" in {
	getErrorMessageForField("name2", view) must beSome("Name must start with a capital letter")
	getErrorMessageForField("password7", view) must beSome("passwords don't match")
	(getInput("name2", view) \ "@class").text must_== "digestive-input-error"
      }
    }

  }
  

}


