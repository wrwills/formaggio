# Scormlets
Scala Formlets

From the [Haskell Wiki](http://www.haskell.org/haskellwiki/Formlets): 
"[Formlets](http://groups.inf.ed.ac.uk/links/formlets/) are a way of building HTML 
forms that are type-safe, handle errors, abstract and are easy to combine into 
bigger forms." The key feature of formlets is that they are composable: formlets
can be combined to produce larger forms using applicative functors.

I started writing this because I thought the idea Formlets was very clever, and I 
was impressed by the Haskell implementations: [Formlets](http://hackage.haskell.org/package/formlets) and 
[Digestive-Functors](http://hackage.haskell.org/package/digestive-functors).  
I also thought it would be a
good way to get to grips with Scalaz which is pretty much essential when trying 
to port ideas from Haskell to Scala.  I generally tried to follow the Haskell implementations, 
especially digestive-functors, but it was necessary to diverge a little in order to 
reuse scalaz' Validators.

Other implementations of the formlets concept include:

 * F#: the (commercial) [WebSharper Platform](http://www.intellifactory.com/products/wsp/Home.aspx)
   contains a [formlets library](http://www.intellifactory.com/docs/formlets/index.html)
 * Racket: (http://docs.racket-lang.org/web-server/formlets.html)
 * Common Lisp: (http://www.cliki.net/admin/edit/edit/Formlets)
 * Links: The original idea for formlets came out of work on Philip Wadler's 
   [Links](http://groups.inf.ed.ac.uk/links/) project.  See this [paper](http://groups.inf.ed.ac.uk/links/formlets/)

This is not meant to be a web framework.  It is concerned solely with form handling.  

## Quick Start ##

 * sbt update
 * sbt jetty
 * Go to (http://localhost:8080/registration) to see the form in action
 * have a look at core/src/main/scala/scormlets/SampleData.scala to see how that page was constructed
 * Have a play on the command line: 
 
     sbt 'project core' console  
     import scormlets.SampleData._   
     val env =  
           Map(  
    	"name1"-> "Jim",   
    	"name2" -> "Bob", 
    	"age3" -> "30",
    	"nickname5" -> "Jimbo",
    	"password7" -> "password",
    	"password6" -> "password"
          )
     runFormState(personForm, env)


## TODO: ##

 * handle file uploads
 * handle radio, multiple checkboxes, drop down selects
 * recaptchas?
 * create examples using lift framework
 * support for different backends eg scalate, commandline
 * i18n support for error messages, field names, etc 
 
 
