package scormlets

import scalaz._

/**
 * Html5 renderings of form components using scala.xml
 *
 * TODO:
 * - radios
 * - selects
 * - mass input
 * - file upload
 */

object Html {
  import Scalaz._
  import Formlets._
  import scala.xml._


  /**
   * base method for adding an input
   * adding an input causes the form state to be modified
   */
  def input[A](name: String, default: Option[A], view: (String,String) => View,
	  validLookup: (Map[String,String], String) => Validation[(String,FormError),A],
	     stringRepresentation: A => String): Form[A] =
    Form(
      (env: Env) =>  
       	for {s <- init[FormState] 
	     val newInt = s._1 + 1
	     val lookupName = name + (s._1 + 1)
	     ns <- modify((x: (FormState)) => (x._1 + 1, lookupName :: x._2))
	   } yield {
	     val valid: Validation[(String,FormError),A] = validLookup(env, lookupName)
	     (valid.liftFailNel, 
	      view(lookupName, 
		   valid.fold(
		     _ match {
		       // empty strings in the environment override default input
		       case (_,EmptyStringError) => ""
		       case  _ => default.map(stringRepresentation(_)).getOrElse("")
		     }, stringRepresentation(_))
		 ) 
	    ) 
	   })
  
  /*
   * standard string input
   * empty strings are not allowed
   *
   * we need to distinguish between a value being there as an empty string in the lookup environment
   * and it not being there for display of default values
   */
  def input(name: String = "sc_", default: Option[String], view: (String,String) => View): Form[String] =
    input(name, default, view, 
	  (env: Map[String,String], lookupName: String) => 
	    env.get(lookupName) match {
	      case Some("") => failure[(String,FormError),String]((lookupName, EmptyStringError))
	      case None => failure[(String,FormError),String]((lookupName, LookupError))
	      case Some(s)  => s.success[(String,FormError)]
	    }, _.toString)

  /**
   * optional input
   * always succeeds even if there is nothing in the lookup environment
   * empty strings in the lookup environment yield Some("")
   */
  def optionalInput(view: (String,String) => View, name: String = "sc_", default: Option[String]): Form[Option[String]] =
    input(name, Some(default), view, 
	  (env: Map[String,String], lookupName: String) => {
	    val lookupResult: Validation[(String,FormError),Option[String]] = 
	      env.get(lookupName) match {
		case Some("") => None.success
		case None     => failure((lookupName, LookupError))
		case s        => s.success
	      }
	    lookupResult
	  }
	  , _.getOrElse(""))
	          
  def errorClassView(error: Boolean ) =  "digestive-input" + (if (error) "-error" else "")

  def inputTextView(password: Boolean) = 
    (name: String, value: String) => 
      (errors: Map[String,String]) =>
	<input type={ if (password) "password" else "text" } name={ name } id={ name } value={ value } 
	 class={ errorClassView( errors.contains(name) ) } />
     
  def optionalInputText(nname: String = "sc_", default: Option[String] = None): Form[Option[String]] =
    optionalInput(inputTextView(false), nname, default)

  def inputText(nname: String = "sc_", default: Option[String] = None, password: Boolean = false): Form[String] =
    input(nname, default, inputTextView(password))

  def inputTextAreaView(cols: Int, rows: Int)  = 
    (name: String, value: String) => 
      (errors: Map[String,String]) => 
	<textarea  name={ name } id={ name } 
	  class={ errorClassView(errors.contains(name) ) }
	  cols={ cols.toString } rows={ rows.toString } >{ value }</textarea>
      
  def inputTextArea(nname: String = "sc_", cols: Int = 40, rows: Int = 3, default: Option[String] = None): Form[String] =
      input(nname, default, inputTextAreaView(cols, rows))

  def optionalInputTextArea(nname: String = "sc_", default: Option[String] = None, cols: Int = 40, rows: Int = 3): Form[Option[String]] =
    optionalInput(inputTextAreaView(cols, rows), nname, default)

  def inputPassword(nname: String = "sc_", default: Option[String] = None): Form[String] =
    inputText(nname, default, true)

  /*
  implicit */
  def validationExceptionToValidationFormError[A](e: Validation[Exception,A]): Validation[FormError,A] =
    e match {
      case Success(s) => success(s)
      case Failure(f) => failure(ExceptionTo(f))
    }

  /**
   * there is no point having checkboxes default to anything but false
   * this is because if a checkbox is not checked no values comes back in the post parameters
   * and it is therefore impossible to distinguish between the box having been set to false
   * and no values being there at all: ie if you set a checkbox to default to true it will
   * always be redisplayed checked
   */
  def inputCheckbox(nname: String = "sc_"): Form[Boolean] = 
    input(
      nname,
      Some(false),
      (name: String, value: String) => ((errors: Map[String,String]) => {
	val input = 
	  <input type="checkbox" name={ name } id={ name } value="true"
	    class={ errorClassView(false) } />
	if (value == "true") 
	  input.copy(attributes = input.attributes.append(new UnprefixedAttribute("checked", "yes", Null)))
	else
	  input
      }), 
      (env: Map[String,String], lookupName: String) => 
	env.get(lookupName).isDefined.success,
      _.toString
    )

  /**
   * add arbitrary html to the form
   */
  def htmlE(view: FormState => View): Form[Unit] = 
    Form(
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
    html((s:FormState) => <label for={ s._2.headOption.getOrElse("unknown") } class="scormlets-label">{ name }</label>)

  def br = html(<br/>)


  /**
   * pull all errors from the form for the current form range and display them
   * reset the form range
   * TODO : make errors lookup
   */    
  def ferrors: Form[Unit] =
    Form(
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
