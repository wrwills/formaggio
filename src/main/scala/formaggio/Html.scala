package formaggio

import scalaz._
import scala.collection.SortedSet

/**
 * Html5 renderings of form components using scala.xml
 *
 * TODO:
 * - hidden
 * - radios
 * - selects
 * - mass input
 * - file upload
 * - recaptcha?
 */


trait Html {
  import Scalaz._
  import Formlets._
  import scala.xml._

  def emptyStringOptional(s: String) = if (s.length < 1) None else Some(s)

  /**
   * doesn't fail if input isn't in the environment map
   * thus an empty string is indistinguishable from nothing being in the environment
   * needed for checkboxes
   * A corollary of this is that default values cannot have any effect on this sort of input
   * http://www.w3.org/TR/html401/interact/forms.html#successful-controls
   */
  def nonFailingInput(
    name: String, 
    view: (String,String) => View 
    //stringRepresentation: String => String = (_: String).toString
  ): Form[Option[String]] = 
    optionalInput(
      name, None, view, x => x.toString, 
      (env,lookupName) => 
	emptyStringOptional(env.get(lookupName).getOrElse("")).success[NonEmptyList[(String,FormError)]])

  /*
  def nonFailingInput[A](
    name: String, view: (String,String) => View, 
    stringRepresentation: A => String = (_: A).toString
  ): Form[Option[String]] = 
    optionalInput(
      name, None, view, stringRepresentation, 
      (env,lookupName) => 
	emptyStringOptional(env.get(lookupName).getOrElse("")).success[NonEmptyList[(String,FormError)]])
        */


  /*
    Form(
      dflt,
      (env: Env) =>  
        for {s <- init[FormState] 
             val newInt = s._1 + 1
             val lookupName = name + "::" + (s._1 + 1)
             ns <- modify((x: FormState) => (x._1 + 1, lookupName :: x._2))
           } yield {
             val dflt = default map (stringRepresentation(_))
             //val lookupValidation: Validation[NonEmptyList[(String, FormError)],Option[String]] = 
//               env.get(lookupName).toSuccess[(String,FormError)]((lookupName,LookupError)).map(emptyStringOptional).liftFailNel
//               env.get(lookupName).toSuccess[(String,FormError)]((lookupName,LookupError)).map(emptyStringOptional).map( _ orElse dflt).liftFailNel
	     val lookup = lookupValidation(env, lookupName)
             (lookup,
              view(lookupName, 
                   lookup | dflt getOrElse("") ) ) 
           }) */
  /*
  def optionalInput[A](
    name: String, default: Option[A], view: (String,A) => View, 
    stringRepresentation: A => String = (_: A).toString, 
    lookupValidation: (Env, String) => Validation[NonEmptyList[(String, FormError)],Option[A]] = 
      (env,lookupName) => env.get(lookupName).toSuccess[(String,FormError)]((lookupName,LookupError)).map(emptyStringOptional).liftFailNel
  ): Form[Option[A]] =
    Form(
      default,
      (env: Env) =>  
        for {s <- init[FormState] 
             val newInt = s._1 + 1
             val lookupName = name + "::" + (s._1 + 1)
             ns <- modify((x: FormState) => (x._1 + 1, lookupName :: x._2))
           } yield {
             val dflt = default map (stringRepresentation(_))
	     val lookup = lookupValidation(env, lookupName)
             (lookup,
              view(lookupName, 
                   lookup | dflt getOrElse("") ) ) 
           })
           */

  /*
   * base method for adding an input
   * Takes care of incrementing the form state and generating a
   * name for the input.
   *
   * most formlets will use this the default lookupValidation
   * If the input is not in the environment map, then we get a LookupError
   * If the input is in the environment but it is an empty string then running the form returns None
   * If the form returns Some[String] then that string is guaranteed to be nonempty
   *
   * Right now just done for strings: TODO parametrize
   */
  def optionalInput(
    name: String, 
    dfalt: Option[String], 
    view: (String,String) => View, 
    stringRepresentation: String => String = (_: String).toString, 
    lookupValidation: (Env, String) => Validation[NonEmptyList[(String, FormError)],Option[String]] = 
      (env,(lookupName: String)) => env.get(lookupName).toSuccess[(String,FormError)]((lookupName,LookupError)).map(emptyStringOptional).liftFailNel
    ): Form[Option[String]] = new Form[Option[String]]{ 
      val default = if (dfalt.isEmpty) None else Some(dfalt)
      val value = 
        (deflt: Option[Option[String]]) =>
        (env: Env) =>  
        for {s <- init[FormState] 
             val newInt = s._1 + 1
             val lookupName = name + "::" + (s._1 + 1)
             ns <- modify((x: FormState) => (x._1 + 1, lookupName :: x._2))
           } yield {
             val dflt = deflt.flatten.headOption map (stringRepresentation(_))
             //val lookupValidation: Validation[NonEmptyList[(String, FormError)],Option[String]] = 
//               env.get(lookupName).toSuccess[(String,FormError)]((lookupName,LookupError)).map(emptyStringOptional).liftFailNel
//               env.get(lookupName).toSuccess[(String,FormError)]((lookupName,LookupError)).map(emptyStringOptional).map( _ orElse dflt).liftFailNel
	     val lookup = lookupValidation(env, lookupName)
             println("getting default: " + dflt)
             val viewVal =
               lookup.toOption.flatten.headOption.getOrElse( dflt.getOrElse("") )
             //  lookup.toOption.flatten.headOption.getOrElse( default.flatten.headOption.getOrElse("") )
             (lookup, view(lookupName, viewVal) )
                   //lookup.toOption.getOrElse( default.flatten.headOption.getOrElse("") ) ) )
                   //lookup | default.flatten.headOption.getOrElse("") ) ) 
           }              
    }

     
  def emailInput(str: String) =
    if (com.recursivity.commons.validator.Email("email", str).isValid)
      str.success
    else
      "Not a valid email address".fail

  def toStringValidation[A](v: Validation[NumberFormatException, A]) =
	 v match { case Success(a) => a.success; case Failure(e) => "number parse fail".fail }

  def doubleInput(nm: String) = toStringValidation(nm.parseDouble) // requiredInput(nm) map ( x => toStringValidation(x.parseDouble) ) 

  def intInput(s: String) = toStringValidation(s.parseInt)

  def inputText(nname: String = "sc_", default: Option[String] = None, inputType: String = "text"): Form[Option[String]] = {
    def inputView(inputType: String) = 
      (name: String, value: String) => 
	(errors: Map[String,String]) =>
	  <input type={ inputType } name={ name } id={ name } value={ value } 
          class={ errorClassView( errors.contains(name) ) } />

    optionalInput(nname, default, inputView(inputType))
  }

  def requiredInput(nname: String = "sc_", default: Option[String] = None, inputType: String = "text"): Form[String] = 
    formNonEmpty( inputText(nname, default, inputType) )

//: Form[String] =
  def inputPassword(nname: String = "sc_password_"): Form[String] = 
    requiredInput( nname, inputType = "password" )

/*
 *     formNonEmpty( inputText(nname, default, "password") )
    {
    val psswd: Form[Validation[FormError,String]] = inputText(nname, default, "password") map (nonEmpty _)
    liftValidation(psswd)
  } */
 //   liftValidation(inputText(nname, default, "password") map (nonEmpty _))

  // hidden input: default value is not optional
  def hidden(nname: String = "sc_", default: String): Form[Option[String]] =
    inputText(nname, Some(default), "hidden")

  def formNonEmpty[A](x : Form[Option[A]]): Form[A] = 
    liftValidation( x map (nonEmpty _))
        	          
  def errorClassView(error: Boolean ) =  "digestive-input" + (if (error) "-error" else "")

  def inputTextAreaView(cols: Int, rows: Int)  = 
    (name: String, value: String) => 
      (errors: Map[String,String]) => 
	<textarea  name={ name } id={ name } 
	  class={ errorClassView(errors.contains(name) ) }
	  cols={ cols.toString } rows={ rows.toString } >{ value }</textarea>
      
  def inputTextArea(nname: String = "sc_", cols: Int = 40, rows: Int = 3, default: Option[String] = None): Form[Option[String]] =
      optionalInput(nname, default, inputTextAreaView(cols, rows))


  /*
  implicit 
  def validationExceptionToValidationFormError[A](e: Validation[Exception,A]): Validation[FormError,A] =
    e match {
      case Success(s) => success(s)
      case Failure(f) => failure(ExceptionTo(f))
    }*/

  /**
   * there is no point having checkboxes default to anything but false
   * this is because if a checkbox is not checked no values comes back in the post parameters
   * and it is therefore impossible to distinguish between the box having been set to false
   * and no values being there at all: ie if you set a checkbox to default to true it will
   * always be redisplayed checked
   * TODO: i think this can be got around using a hack with multiple checkboxes
   */
  def inputCheckbox(nname: String = "sc_"): Form[Boolean] = {
    val chkbx = 
      nonFailingInput(nname,
	(name: String, value: String) => 
	  ((errors: Map[String,String]) => {
	    val input = 
	      <input type="checkbox" name={ name } id={ name } value="true"
	    class={ errorClassView(false) } />
	    if (value == "true") 
	      input.copy(attributes = input.attributes.append(new UnprefixedAttribute("checked", "yes", Null)))
	    else
	      input
	  }))
    def toBool(x: Option[String]) = 
      x match {
	case Some("true") => true
	case _ => false
      }
    chkbx ∘ toBool _    
  }
  /*
  def inputCheckbox2(nname: String = "sc_", default: Boolean = false): Form[Boolean] = {
    val chkbx = 
      optionalInput(nname,
	(name: String, value: String) => 
	  ((errors: Map[String,String]) => {
	    val input = 
	      <input type="checkbox" name={ name } id={ name } value="true"
                     class={ errorClassView(false) } />
	      <input type="checkbox" name={ name } id={ name } value="true"
                     class={ errorClassView(false) } />	      
	    if (value == "true") 
	      input.copy(attributes = input.attributes.append(new UnprefixedAttribute("checked", "yes", Null)))
	    else
	      input
	  }))
    def toBool(x: Option[String]) = 
      x match {
	case Some("true") => true
	case _ => false
      }
    chkbx ∘ toBool _    
  }*/


  // todo:
  // def checkboxChoice(nname: String = "sc_"): Form[Boolean] = 

  /*
   * 
   * 2nd part of options  should be unique and non-empty strings
   * todo: layout options - how many columns?
   */
  def mkCheckedInputs(ttype: String, name: String, options: Seq[(String, String)], selectedValue: String): Seq[NodeSeq] = {
    def mkInput( iname: String) = {
      val input =
	<input type={ ttype } name={ name } id={ iname } value={ iname }/>
      if (iname == selectedValue)
	input.copy(attributes = input.attributes.append(new UnprefixedAttribute("checked", "yes", Null)))
      else
	input
    }	
    options.map( (x: (String,String)) => labelHtml(x._2, x._1) ++ mkInput( x._2 ) )
  } 
    

  private def radio2(nname: String, options: SortedSet[String], default: Option[String]): Form[Option[String]] = {
    val opts = options.toSeq.map(x => (x, x))
    optionalInput(nname, default,
	  (name: String, value: String) => 
	    ((errors: Map[String,String]) =>
	      mkCheckedInputs("radio", nname, opts, value).flatten ) )
  }

  /*
   * radio choice -- only one is selected
   * todo: look at differences between html4 and html5 for this
   * 2nd part of options  should be unique
   * If there is a default value then the formlet is guaranteed to return something
   */  
  def radio(nname: String = "sc_", options: SortedSet[String]): Form[Option[String]] =
    radio2(nname, options, None)

  def radio(nname: String, options: SortedSet[String], default: String): Form[String] = 
    formNonEmpty( radio2(nname, options, Some(default)) )
  // a radio choice  
  
  //def radio(nname: String = "sc_", options: Seq[String], default: Option[String]): Form[String] =
  //  radio2(nname, options.map(x => (x, x)), default ) 

  def toEnumerationValue[A](vals: Set[A], errorMessage: String): String => Validation[String,A] =
    (str: String) =>
      vals.toSeq.filter( _.toString != "Value").filter(_.toString == str).headOption.toSuccess(errorMessage)

  // must return somethin
  def radioEnumeration[A](vals: SortedSet[A], nname: String = "sc_", default: A): Form[A] = 
    validate(
      radio(nname + "enum", vals.map(_.toString), default.toString) ∘ 
      toEnumerationValue(vals.toSet, "not in enumeration"))
 
  
  /**
   * add arbitrary html to the form
   */
  def htmlE(view: FormState => View): Form[Unit] = 
    Form(
      (_: Option[Unit]) =>
      (env: Env) =>  
	for {s <- init[FormState]} yield (success(()), view(s)) 
    )
  
  def htmlV(view: View) =
    htmlE( (s:FormState) => view )

  def html(n: FormState => NodeSeq): Form[Unit] = 
    htmlE( (s:FormState) => (errors: Map[String,String]) => n(s) )

  def html(n: NodeSeq): Form[Unit] = html((_:FormState) => n)
    
  /**
   * add an html5 label to the left of an input
   * the for attribute will use the id of the input to the right
   */
  def label(name: String): Form[Unit] = 
    html((s:FormState) => labelHtml(s._2.headOption.getOrElse("unknown"), name))

//<label for={ s._2.headOption.getOrElse("unknown") } class="scormlets-label">{ name }</label>)

  def labelHtml(forr: String, name: String) =
    <label for={ forr } class="scormlets-label">{ name }</label>

  def br = html(<br/>)


  /**
   * pull all errors from the form for the current form range and display them
   * reset the form range
   * TODO : make errors lookup
   */    
  def ferrors: Form[Unit] =
    Form(
      (dflt: Option[Unit]) =>
      (env: Env) =>  
	for {s <- init[FormState]
	     _ <- modify((x: (FormState)) => (x._1, List[String]()))
	   } yield {
	     val lab = 
	       (errors: Map[String,String]) => {
		 val errorsForRange = 
		   for {field <- s._2
			er    <- errors.get(field)
		      } yield (field,er)
		 if (errorsForRange.size > 0)
		   errorHtml(errorsForRange)
		 else 
		   Text("")
	       }
	     ( success(()),  lab )
	   }
    )

  def errorHtml(errors: Seq[(String,String)]) =
    <ul class="scormlets-errors">{ 
      errors map ((e:(String,String)) => (<li id={e._1}>{ e._2 }</li>))
    }</ul>
    
  def allErrors = 
    htmlV(
      (errors: Map[String,String]) => 
	if (errors.size > 0) 
	  <h1>Errors:</h1> ++  errorHtml(errors.toSeq)
	else
	  Text(""))
	  
 
}
