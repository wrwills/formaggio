package scormlets

import scalaz._


/**
 * keep these in core so they can be reused across sample apps and testing
 *
 * NB:  scalaz provides unicode operators which define common methods for higher order types
 *      In the interest of not scaring of potential users who just want to use this library
 *      without getting deeply into scalaz I have chosen to use the ascii equivalents here
 * 
 * For those who prefer the unicode operators the relevant conversions are:     
 *    map -> ∘
 *    |@| -> ⊛
 * 
 */
object SampleData {
  import Scalaz._
  import Formlets._
  import Html._
  import scala.xml._
  

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

  val myForm = (inputText("first") |@| inputText("last")){FullName2(_,_)}
  val myNameForm: Form[Name] = validate(inputText("name") map Name.apply )
  val fullNameForm = (myNameForm  |@| myNameForm){  FullName(_,_) }

  def labelledNameForm(s: String) =  validate(label(s) ++> (inputText("name", Some("Billy")) map Name.apply))

  val labelledFullNameForm = (labelledNameForm("First")  |@| labelledNameForm("Second")){ FullName(_,_) } <++ ferrors

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

  val ageForm = validate(label("Age") ++> (inputText("age") map Age.apply))

  object FavouriteFood extends Enumeration {
    type FavouriteFood = Value
    val GreenEggs, Ham = Value
  }

  // TODO: check that can handle optional values without having entire form fail
  case class Person(
    name: FullName, age: Age, married: Boolean, nickname: Option[String], 
    password: String, favourites: Favourites)

  import FavouriteFood._

  case class Favourites(food: FavouriteFood, things: Seq[String])

  val noFavourites = Favourites(GreenEggs, Seq())



  val favouritesForm1 = radio("food", Seq("GreenEggs", "Ham"), None) map ((x:String) => FavouriteFood.withName(x))

  val favouriteFoodsForm: Form[FavouriteFood] = (label("Favourite food:") ++> radioEnumeration(FavouriteFood.values))

  val favouriteThings: Form[Seq[String]] = 
    label("Favourite things:") ++> massInput(inputText())
  
  //val favouritesForm = (label("Favourite food:") ++> radioEnumeration(FavouriteFood.values)){ Favourites((_:FavouriteFood),Seq()) }

  def mkPerson(name: FullName, age: Age, married: Boolean, nickname: Option[String], password: String, terms: Boolean, favourites: Favourites) =
    Person(name, age, married, nickname, password, favourites)

  def passwordVerify(x: String)(y: String): Validation[String, String] = 
    if (x == y) x.success[String] else failure[String,String]("passwords don't match")

  // TODO: numbering for this is coming out backwards 
  // doesn't matter for now but need to look at ordering of fields using different notations  
  val passwordValidation = 
    ((label("Password") ++> inputPassword("password")) |@| 
    (label("Password (verify)") ++> (inputPassword("password")))){ passwordVerify(_)(_) }

/*
  val passwordValidation = (label("Password") ++> inputPassword("password")  |@| 
					     (label("Password (verify)") ++> (inputPassword("password"))
					     map passwordVerify ) ) )*/
//: Validation[String,Boolean] = 
  def requireTrue(errorMessage: String)(x: Boolean) =
    if (x) x.success else failure(errorMessage)

  val termsAndConditions =   
    validate(label("Do you accept our terms and conditions?:") ++>
    (inputCheckbox("terms") map requireTrue("You must accept our outrageous terms and conditions!")))

  val personForm = 
    (
      (allErrors ++> 
       labelledFullNameForm <++ br) |@| 
      (ageForm <++ ferrors <++ br) |@| 
      (label("Married:") ++> inputCheckbox("married") <++ br) |@| 
      (label("Nickname:") ++> optionalInputText("nickname") <++ br) |@|
      (validate(passwordValidation) <++ ferrors <++ br) |@|	
      (termsAndConditions <++ ferrors <++ br)      
    ){ mkPerson(_,_,_,_,_,_,noFavourites) }



}
