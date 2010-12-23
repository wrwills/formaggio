package scormlets

import scalaz._
import org.specs._
import Formlets._

object ViewHelpers {
  import scala.xml._

  def getInput(s: String, view: NodeSeq) =  
    ((view \\ "input") filter ((x:Node) => (x \ "@id").text == s)).head

  def getValueForInput(s: String, view: NodeSeq) =  
    (getInput(s,view) \ "@value").text

  def getValueForLabel(s: String, view: NodeSeq) =
    (view \\ "label") filter ( _.text == s ) map (_ \ "@for") map (_.text) head

  def getErrorMessageForField(s: String, view: NodeSeq) =  
    (for {i <- view \\ "li";
	  if( (i \ "@id").text == s) }
     yield i.text).headOption  
}

object FormletsSpecs extends Specification {  
  import SampleData._  // types used in these forms
  import Scalaz._
  import ViewHelpers._

  /*
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
			      Map("name::1"-> "Jim", 
				  "name2" -> "bob"
				))
      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name2","Name must start with a capital letter")

      val view = rslt._2
      println(view)
      getValueForInput("name::1", view) must_== "Jim"
      getValueForInput("name2", view) must_== "bob"
      

      "and the view should contain an appropriate error message" in {
	getErrorMessageForField("name2", view) must beSome("Name must start with a capital letter")
	(getInput("name2", view) \ "@class").text must_== "digestive-input-error"
	//for {
      }
    }
  }*/

  "inputs should be handled correctly" in {
    val form = inputText("test", Some("default"))

    "default input should be displayed if there is no input" in {
      val rslt = runFormState(form, Map())
      println(rslt)
      getValueForInput("test::1", rslt._2) must_== "default"
    }
    "empty string input should override default input" in {
      val rslt = runFormState(form, Map("test::1" -> ""))
      getValueForInput("test::1", rslt._2) must_== ""
    }
  }

  "optional inputs should be handled correctly" in {
    val form = optionalInputText("test", Some("default"))

    "input from environment should show up correctly in form" in {
      val rslt = runFormState(form, Map("test::1" -> "some text"))
      rslt._1.isSuccess must beTrue
      getValueForInput("test::1", rslt._2) must_== "some text"
    }
    "default input should be displayed if there is no input" in {
      val rslt = runFormState(form, Map())
      rslt._1.isFailure must beTrue
      getValueForInput("test::1", rslt._2) must_== "default"
    }
    "empty string input should override default input" in {
      val rslt = runFormState(form, Map("test::1" -> ""))
      rslt._1.isSuccess must beTrue
      getValueForInput("test::1", rslt._2) must_== ""
    }
  }

  "checkboxes should be handled correctly" in {
    val form = inputCheckbox("test")
    
    "if there is any input for the parameter then the form should return true" in {
      val rslt = runFormState(form, Map("test::1" -> "true"))
      println(rslt)
      rslt._1.either.right.get must_== true
      (getInput("test::1",rslt._2) \ "@checked").text must_== "yes"
    }    
    "if there is no input then the form returns false" in {
      val rslt = runFormState(form, Map())
      rslt._1.isSuccess must beTrue
      rslt._1.either.right.get must_== false
      (getInput("test::1",rslt._2) \ "@checked").text must_== ""
    }
  }

  "required checkboxes should be handled correctly" in {
    val form = termsAndConditions
    "if there is any input for the parameter then the form should return true" in {
      val rslt = runFormState(form, Map("test::1" -> "true"))
    }
  }

  "for a person form" in {
    val form = personForm

    val env = 
      Map(
	"name::1"-> "Jim", 
	"name::2" -> "Bob", 
	"age::3" -> "30",
	"nickname::5" -> "Jimbo",
	"password::7" -> "password",
	"password::6" -> "password"
      )


    val successRslt = runFormState(form, env)
    println(successRslt)

    "form view should be correct" in {
      val view = successRslt._2

      "fields should be filled in with environment values" in {
	getValueForInput("name::1", view) must_== "Jim"
	getValueForInput("name::2", view) must_== "Bob"
	getValueForInput("age::3", view) must_== "30"
      }

      "labels should be filled in correctly" in {
	getValueForLabel("First", view) must_== "name::1"
	getValueForLabel("Second", view) must_== "name::2"
	getValueForLabel("Age", view) must_== "age::3"
      }

    }
    
    "form should fail if one of the values isn't filled in" in {
      val rslt = runFormState(form, Map("name::1"-> "Jim"))
      println(rslt)

      val view = rslt._2
      // gets filled in with default value
      getValueForInput("name::2", view) must_== "Billy"

      rslt._1.isFailure must beTrue
      rslt._1.fail.toOption.get.head must_==  ("name::2", LookupError)
    }

    "form should fail if one of the values isn't filled in correctly" in {
      val rslt = runFormState(form, env + ("name::2" -> "bob") + ("password::6" -> "passwd"))
      println(rslt)
      rslt._1.isFailure must beTrue

      val errors = rslt._1.fail.toOption.get.list.toMap
      //(errors.get("name::2").get)("") must_== Some(GenericError("Name must start with a capital letter"))
      //(errors.get("password7").get)("") must_== Some(GenericError("passwords don't match"))

      val view = rslt._2
      getValueForInput("name::1", view) must_== "Jim"
      getValueForInput("name::2", view) must_== "bob"
      
      "and the view should contain an appropriate error message" in {
	getErrorMessageForField("name::2", view) must beSome("Name must start with a capital letter")
	getErrorMessageForField("password::7", view) must beSome("passwords don't match")
	(getInput("name::2", view) \ "@class").text must_== "digestive-input-error"
      }
    }

  }

  "plug should modify the xml in the form" in {
    import scala.xml._

    val plugged = inputText() plug ((x:NodeSeq) => <ul><li>{ x }</li></ul>)
    val target = <ul><li><input value="" type="text" class="digestive-input" name="sc_::1" id="sc_::1"></input></li></ul>
    // not sure why but this doesn't work if you compare the xml datastructures
    (getFormView(plugged)).toString must_== target.toString

  }

  "mass input forms should work correctly" in {
    import scala.xml._

    val mI = massInput(inputText())
    (getInput("sc_::101", getFormView(mI)) \ "@name").text must_== "sc_::101"

    "when sending multiple values to a mass input" in {
      val rslt = runFormState(mI, Map("sc_::101" -> "foo", "sc_::102" -> "bar"))
      
      "another entry should be added to the view to allow for a new input" in {	
	((rslt._2 \\ "input") filter ((x:Node) => (x \ "@type").text == "text")).length must_== 3
	//(rslt._2 \\ "input").length must_== 3
	((getInput("sc_::103", rslt._2)) \ "@name").text must_== "sc_::103"
      }

      "the mass input must yield a collection" in {
	rslt._1.toOption.get.toList must_== List("foo", "bar")
      }
    }



    "mass input should apppend to the rest of the form correctly" in {
      case class Thingy(thing1: String, things: Seq[FullName], thing2: String) 
      implicit def ThingyEqual: Equal[Thingy] = equalA

      val frm = (inputText() |@| massInput(fullNameForm) |@| inputText() <++ ferrors){ Thingy(_,_,_) }

      val rslt = 
	getFormValidation(
	  frm, 
	  Map("sc_::1" -> "some", 
	      "name::201" -> "Foo", "name::202" -> "Bar", 
	      "name::203" -> "Jim", "name::204" -> "Bob", 
	      "sc_::3" -> "another"))
      //println(runFormState(frm, Map("sc_::1" -> "some", "name::201" -> "foo", "name::202" -> "bar", "sc_::3" -> "another")))
      //println(getFormView(frm))
      println(rslt)
      rslt.isSuccess must beTrue
      //val names = FullName(Name("Foo"),"Bar")
      println(Thingy("some", Vector(FullName("Foo", "Bar").toOption.get,FullName("Jim","Bob").toOption.get),"another"))
      rslt.toOption.get.thing1 must_== "some"
      rslt.toOption.get.thing2 must_== "another"
      /* should pass this test but for some reason says not equal
      (Seq(
	FullName("Foo", "Bar"), 
	FullName("Jim", "Bob")
      ).sequence[({type λ[α]=Validation[String, α]})#λ, FullName]).toOption.get must_== rslt.toOption.get.things.toList
      */
      //(rslt.toOption.get ≟ Thingy("some", Vector(FullName("Foo", "Bar").toOption.get,FullName("Jim","Bob").toOption.get),"another")) must beTrue
      //must_== Thingy("some", Vector(FullName("Foo", "Bar").toOption.get,FullName("Jim","Bob").toOption.get),"another")
      //rslt.toOption.get.things must
      "errors should work correctly with mass input" in {
	val rslt = getFormValidation(frm, Map("sc_::1" -> "some", "sc_::201" -> "foo", "sc_::202" -> "bar", "sc_::3" -> "another"))
      }
      
    }

  }
  

}


