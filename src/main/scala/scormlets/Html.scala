package scormlets

import scalaz._

object Html {
  import Scalaz._
  import Formlets._


  def input(name: String): Form[String] =
    Form(
      (env: Env) =>  
       	for {s <- init[Int] 
	     _ <- modify((_: Int) + 1)
	   } yield {
	     val lookupName = name + s
	     val lookup = env.get(name + s)
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
	for {s <- init[Int]} yield 
	  {
	    val lab = 
	      (errors: Map[String,String]) => <label for={ "name" + s } class="digestive-label">{ name }</label>
	    ( success(()),  lab )
	  }
    )


  

  /*
   def inputN(name: String): Form[Name] =
   Form(
   (env: Env) =>  
   for {s <- init[Int] 
   _ <- modify((_: Int) + 1)
   } yield {
   val lookupName = name + s
   println("lookupName " + lookupName)
   val valid: Validation[String,Name] =
   for (a <- env.get(lookupName).toSuccess[String]("could not lookup for " + lookupName);
   b <- Name(a)) yield b
   val view =
   (errors: Map[String,String]) => 
   <input type="text" name={ lookupName } id={ lookupName } value={ errors.get(lookupName).toString } class="digestive-input" />
   (valid ,view)
   })*/


  /*
   def inputText[A](name: String = "")(implicit f: FromString[A]): Form[A] =
   Form(
   (env: Env) =>  
   for {s <- init[Int] 
   _ <- modify((_: Int) + 1)
   } yield {
   val lookupName = name + s
   println("lookupName " + lookupName)
   val valid =
   env.get(lookupName).toSuccess[NonEmptyList[(String,String)]](
   nel((lookupName, "could not lookup for " + name),List())).fromString
   val view =
   (errors: Map[String,String]) => 
   <input type="text" name={ lookupName } id={ lookupName } value={ errors.get(lookupName).toString } class="digestive-input" />
   (valid,view)
   }
   )
   */

  //def inputAndLabel(name: String): Form[String] =
}
