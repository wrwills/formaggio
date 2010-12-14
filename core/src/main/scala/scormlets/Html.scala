package scormlets

import scalaz._

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
	     ns <- modify((x: FormState) => (x._1 + 1, lookupName :: x._2))
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

  def inputTextView(inputType: String = "text") = 
    (name: String, value: String) => 
      (errors: Map[String,String]) =>
	<input type={ inputType } name={ name } id={ name } value={ value } 
	 class={ errorClassView( errors.contains(name) ) } />

     
  def optionalInputText(nname: String = "sc_", default: Option[String] = None): Form[Option[String]] =
    optionalInput(inputTextView(), nname, default)

  def inputText(nname: String = "sc_", default: Option[String] = None, inputType: String = "text"): Form[String] =
    input(nname, default, inputTextView(inputType))

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
    inputText(nname, default, "password")

  // hidden input: default value is not optional
  def hidden(nname: String = "sc_", default: String) =
    inputText(nname, Some(default), "hidden")
  

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
    

  /*
   * radio choice -- only one is selected
   * todo: look at differences between html4 and html5 for this
   * 2nd part of options  should be unique
   */  
  def radio2(nname: String, options: Seq[(String,String)], default: Option[String]): Form[String] = 
    input(
      nname,
      default,
      (name: String, value: String) => 
	((errors: Map[String,String]) =>
	  mkCheckedInputs("radio", nname, options, value).flatten ) )

  // a radio choice  
  
  def radio(nname: String = "sc_", options: Seq[String], default: Option[String]): Form[String] =
    radio2(nname, options.map(x => (x, x)), default ) 

  def toEnumerationValue[A](vals: Set[A], errorMessage: String): String => Validation[String,A] =
    (str: String) =>
      vals.toSeq.filter( _.toString != "Value").filter(_.toString == str).headOption.toSuccess(errorMessage)

  def radioEnumeration[A](vals: Set[A], nname: String = "sc_"): Form[A] = 
    validate(
      radio(nname + "enum", vals.toSeq.map(_.toString), vals.headOption.map(_.toString)) ∘ 
      toEnumerationValue(vals, "not in enumeration"))

  

  //import scala.{ValueSet,Value}

  //def radio[A <: Enumeration](nname: String = "sc_", options: Set[A], default: Option[A.Value]) = 
  //  radio2(nname, options.map(_.toString).filter(_ != "Value"), default.map(_.toString) )

// do we really need to lowercase, etc?
//    radio2(nname, options.map(x => (x, x.toLowerCase.replace(' ', '_')) ), default )

  /*
  def validateEnumeration[A < Enumeration](x: A, v: String): Validation[String,A] =
    try {
      x.withName(v).success
    } catch {
      case e: failure("no such element " + v  + " in enumeration")
    } */

//  def massInput[A](vals: Seq[A], nname: String = "sc_"): Seq[A] = 
//    input(nname, 

  	  //ns <- modify((x: (FormState)) => (origS._1 * 100, origS._2) )
	  //nnss <- modify((x: (FormState)) => (origS._1 + 1, nns._2) )
//	  ns <- modify( (x: FormState) => (s._1, x._2 ++ s._2) )
  /**
   * a formlet for inputting lists of things
   * : Form[Seq[A]] = 
   */
  def massInput[A](formlet: Form[A], 
		   itemWrapper: NodeSeq => NodeSeq, 
		   listWrapper: NodeSeq => NodeSeq, default: Seq[A]): Form[Seq[A]] =
    Form(
      (env: Env) =>  
	for {
	  s <- init[FormState];
	  _ <- put( (s._1 * 100, List[String]()) );
	  val mI = ((1 until 3) map ( _ => formlet)) sequence;
	  ns <- modify( (x: FormState) => (s._1 + 1, x._2 ++ s._2) )
	} yield {
	  //val frm = mI(env)
	  val frm = mI(env) ! (100, List())
	  frm
	})

//	  val origS = s
	  //_ <- put( (origS._1 * 100, Seq()) );
	  //val mI = ((1 until 3) map ( _ => formlet)) sequence
	  //nns <- init[FormState];
	  //nnss <- put((origS._1, origS._2 ++ nns._2))

		     /*
  def massInput[A](formlet: Form[A], 
		   itemWrapper: NodeSeq => NodeSeq, 
		   listWrapper: NodeSeq => NodeSeq, default: Seq[A]): Form[Seq[A]] =
    Form(
      (env: Env) =>
	for {
	  s <- init[FormState] 
	} yield {
	  def massInputEnvironment(state: FormState, vals: Seq[]) = 
	    env.get(state._2.head) match {
	      case Some(nm) => 
	  }
	  val num = s._1 * 100
	  val xml = 
	    formlet(Map())
	}) 

  def massInputEnvironment[A](formlet: Form[A], env: Env, state: FormState): Seq[String] = {
    
  }*/
    

  val jsMassInputCode =
  Seq("function findItems(button) {"
  ,"  var mainDiv = $(button).parent();"
  ,"  while ( !mainDiv.hasClass('massInput') ) {"
  ,"    mainDiv = $(mainDiv).parent();"
  ,"  }"
  ,"  return $('.massInputItem', mainDiv);"
  ,"}"
  ,"function addItem(button) {"
  ,"  var items = findItems(button);"
  ,"  var item = $(items[items.length-1]);"
  ,"  var newItem = item.clone(true);"
  ,""
  ,"  newItem.html(newItem.html().replace(/fval\\[(\\d+\\.)*(\\d+)\\.(\\d+)\\]/g, "
  ,"    function(a, b, c, d) {"
  ,"      var newC = parseInt(c)+1;"
  ,"      return a.replace(/\\d+\\.\\d+\\]/, newC+'.'+d+']');"
  ,"    }"
  ,"  ));"
  ,""
  ,"  newItem.children('input').attr('value','');"
  ,"  newItem.appendTo(item.parent());"
  ,"}"
  ,"function removeItem(button) {"
  ,"  var items = findItems(button);"
  ,"  if ( items.length > 1 ) {"
  ,"    var item = $(items[items.length-1]);"
  ,"    item.remove();"
  ,"  } else {"
  ,"    alert('Cannot remove any more rows');"
  ,"  }"
  ,"}").map(_ + "\n").foldLeft("")(((x:String),(y:String)) => x + y)

  
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
