package scormlets

import scalaz._

object Html {
  import Scalaz._
  import Formlets._

  def input(name: String): Form[String] =
    Form(
      (env: Env) =>  
       	for {s <- init[FormState] 
	     val newInt = s._1 + 1
	     val lookupName = name + (s._1 + 1)
	     ns <- modify((x: (FormState)) => (x._1 + 1, lookupName :: x._2))
	   } yield {
	     println("input state: " + ns)
	     //val lookupName = name + s._1
	     val lookup = env.get(lookupName)
	     val valid =
               lookup.toSuccess[NonEmptyList[(String,String)]](
		 nel((lookupName, "could not lookup for " + name),List()))
	     val view =
	       (errors: Map[String,String]) => 
		 <input type="text" name={ lookupName } id={ lookupName } value={ lookup.getOrElse("") } class="digestive-input" />
	     (valid,view)
	   })

  // need to modify state monad to be (String,Int) for this to work
  def label(name: String): Form[Unit] = 
    Form(
      (env: Env) =>  
	for {s <- init[FormState]} yield 
	  {
	    println("label state: " + s) 
	    val lab = 
	      (errors: Map[String,String]) => <label for={ s._2.headOption.getOrElse("unknown") } class="scormlets-label">{ name }</label>
	    ( success(()),  lab )
	  })


  /**
   * pull all errors from the form for the current form range and display them
   * reset the form range
   */  
  def errors: Form[Unit] =
    Form(
      (env: Env) =>  
	for {s <- init[FormState]
	     _ <- modify((x: (FormState)) => (x._1 + 1, List[String]()))
	   } yield {
	     val lab = 
	       (errors: Map[String,String]) => 
		 <ul class="scormlets-errors">{ 
		   s._2 map ((x: String) => <li>{x}</li>) 
		 }</ul>
	     ( success(()),  lab )
	   }
    )

}

