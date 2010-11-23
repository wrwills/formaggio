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

	     	     
  def input[A](name: String, view: (String,String) => View,
	  validLookup: (Map[String,String], String) => Validation[(String,String),A]): Form[A] =
    Form(
      (env: Env) =>  
       	for {s <- init[FormState] 
	     val newInt = s._1 + 1
	     val lookupName = name + (s._1 + 1)
	     ns <- modify((x: (FormState)) => (x._1 + 1, lookupName :: x._2))
	   } yield {
	     val valid: Validation[(String,String),A] = validLookup(env, lookupName)
	     (valid.liftFailNel, view(lookupName, 
				      valid match { 
					case Success(s) => s.toString 
					case Failure(_) => "" 
				      } ) )
	   })
  
  /*
   * standard string input
   * empty strings are not allowed
   */
  def input(name: String = "sc_", view: (String,String) => View): Form[String] =
    input(name, view, 
	  (env: Map[String,String], lookupName: String) => 
	    env.getOrElse(lookupName, "") match {
	      case "" => failure[(String,String),String]((lookupName, name + " can not be empty"))
	      case s  => s.success[(String,String)]
	    })

  def optionalInput(view: (String,String) => View, name: String = "sc_"): Form[Option[String]] =
    input(name, view, 
	  (env: Map[String,String], lookupName: String) => 
	    (env.getOrElse(lookupName, "") match {
	      case "" => None
	      case s  => Some(s)
	    }).success[(String,String)] )
	  
        
  def errorClassView(error: Boolean ) =  "digestive-input" + (if (error) "-error" else "")

  val inputTextView = 
    (name: String, value: String) => 
      ((errors: Map[String,String]) => 
	<input type="text" name={ name } id={ name } 
       value={ value } 
       class={ errorClassView( errors.contains(name) ) } />) 

  def optionalInputText(nname: String = "sc_"): Form[Option[String]] =
    optionalInput(inputTextView, nname)

  def inputText(nname: String = "sc_", password: Boolean = false): Form[String] =
    input(nname, inputTextView)

  def inputTextAreaView(cols: Int, rows: Int)  = 
    (name: String, value: String) => 
      ((errors: Map[String,String]) => 
	<textarea  name={ name } id={ name } 
       class={ errorClassView(errors.contains(name) ) }
       cols={ cols.toString } rows={ rows.toString } >{ value }</textarea>)

      def inputTextArea(nname: String = "sc_", cols: Int = 40, rows: Int = 3): Form[String] =
        input(nname, inputTextAreaView(cols, rows))

  def optionalInputTextArea(nname: String = "sc_", cols: Int = 40, rows: Int = 3): Form[Option[String]] =
    optionalInput(inputTextAreaView(cols, rows), nname)

  def inputPassword(nname: String = "sc_"): Form[String] =
    inputText(nname, true)

  def inputCheckbox(nname: String = "sc_"): Form[Boolean] =
    input(
      nname,	       
      (name: String, value: String) => ((errors: Map[String,String]) => 
	<input type="checkbox" name={ name } id={ name } 
					value={ value } 
					class={ errorClassView(false) } />), 
      (env: Map[String,String], lookupName: String) => 
	liftExceptionValidation(for {
	  x <- env.get(lookupName).toSuccess[Exception](LookupException(lookupName));
	  y <- x.parseBoolean
	} yield y)
    )
         
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
