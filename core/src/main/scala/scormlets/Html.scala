package scormlets

import scalaz._

/**
 * Html5 renderings of form components using scala.xml
 *
 * TODO:
 * - allow optional default values
 * - mass input
 * - file upload
 */

object Html {
  import Scalaz._
  import Formlets._
  import scala.xml._


  /**
   *
   */
  def input[A](name: String, default: Option[A], view: (String,String) => View,
	  validLookup: (Map[String,String], String) => Validation[(String,FormError),A]): Form[A] =
    Form(
      (env: Env) =>  
       	for {s <- init[FormState] 
	     val newInt = s._1 + 1
	     val lookupName = name + (s._1 + 1)
	     ns <- modify((x: (FormState)) => (x._1 + 1, lookupName :: x._2))
	   } yield {
	     val valid: Validation[(String,FormError),A] = validLookup(env, lookupName)
	     (valid.liftFailNel, view(lookupName, 
				      valid match { 
					case Success(s) => s.toString 
					case Failure(_) => default.map(_.toString).getOrElse("")
				      } ) )
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
	    //env.getOrElse(lookupName, "") match {
	    env.get(lookupName) match {
	      case Some("") => failure[(String,FormError),String]((lookupName, EmptyStringError))
	      case None => failure[(String,FormError),String]((lookupName, LookupError))
	      case Some(s)  => s.success[(String,FormError)]
	    })

  def optionalInput(view: (String,String) => View, name: String = "sc_", default: Option[String]): Form[Option[String]] =
    input(name, None, view, 
	  (env: Map[String,String], lookupName: String) => 
	    Some((env.get(lookupName) match {
	      case Some("") => default.getOrElse("")
	      case Some(s)  => s
	      case _        => ""
	    })).success[(String,FormError)] )
	  
        
  def errorClassView(error: Boolean ) =  "digestive-input" + (if (error) "-error" else "")

  val inputTextView = 
    (name: String, value: String) => 
      ((errors: Map[String,String]) => 
	<input type="text" name={ name } id={ name } 
       value={ value } 
       class={ errorClassView( errors.contains(name) ) } />) 

  def optionalInputText(nname: String = "sc_", default: Option[String] = None): Form[Option[String]] =
    optionalInput(inputTextView, nname, default)

  def inputText(nname: String = "sc_", default: Option[String] = None, password: Boolean = false): Form[String] =
    input(nname, default, inputTextView)

  def inputTextAreaView(cols: Int, rows: Int)  = 
    ((name: String, value: String) => 
      ((errors: Map[String,String]) => 
	<textarea  name={ name } id={ name } 
       class={ errorClassView(errors.contains(name) ) }
       cols={ cols.toString } rows={ rows.toString } >{ value }</textarea>))
      
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


  def inputCheckbox(nname: String = "sc_", default: Boolean = false): Form[Boolean] =
    input(
      nname,
      Some(default),
      (name: String, value: String) => ((errors: Map[String,String]) => 
	<input type="checkbox" name={ name } id={ name } 
					value={ value } 
					class={ errorClassView(false) } />), 
      (env: Map[String,String], lookupName: String) => 
	addIndexToValidation(
	  lookupName, 
	  for {
	    x <- env.get(lookupName).toSuccess[FormError](LookupError);
	    y <- validationExceptionToValidationFormError(x.parseBoolean)
	  } yield y)
    )

/*
	liftExceptionValidation(for {
	  x <- env.get(lookupName).toSuccess[FormError](LookupError);
	  y <- validationExceptionToValidationFormError(x.parseBoolean)
	} yield y)
*/
/*
    Validation[
FormException {
    val value = e
  } */

         
  /**
   * add an html5 label to the left of an input
   * the for attribute will use the id of the input to the right
   */
  def label(name: String): Form[Unit] = 
    Form(
      (env: Env) =>  
	for {s <- init[FormState]} yield 
	  {
	    val lab = 
	      (errors: Map[String,String]) => <label for={ s._2.headOption.getOrElse("unknown") } class="scormlets-label">{ name }</label>
	    ( success(()),  lab )
	  })


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
		   <ul class="scormlets-errors">{ 
		     errorsForRange map ((e:(String,String)) => (<li id={e._1}>{ e._2 }</li>))
		   }</ul>
		 else 
		   Text("")
	       }
	     ( success(()),  lab )
	   }
    )

}
