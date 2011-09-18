# Formaggio
## Formlets for Scala

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

There are implementations of the formlets concept in several languages including:

 * Haskell: [Formlets](http://hackage.haskell.org/package/formlets), [Digestive-Functors](http://hackage.haskell.org/package/digestive-functors), and [Yesod](http://www.yesodweb.com/)
 * F#: the (commercial) [WebSharper Platform](http://www.intellifactory.com/products/wsp/Home.aspx)
   contains a [formlets library](http://www.intellifactory.com/docs/formlets/index.html)
 * Racket: (http://docs.racket-lang.org/web-server/formlets.html)
 * Common Lisp: (http://www.cliki.net/admin/edit/edit/Formlets)
 * Links: The original idea for formlets came out of work on Philip Wadler's 
   [Links](http://groups.inf.ed.ac.uk/links/) project.  See this [paper](http://groups.inf.ed.ac.uk/links/formlets/)

This is not meant to be a web framework.  It is concerned solely with form handling.  

## Using in your project

See (https://github.com/wrwills/formaggio-examples) for an example of using Formaggio in a project.

    "formaggio repo" at "http://wrwills.github.com/formaggio/repository/"
    "com.github.wrwills" %% "formaggio" % "0.2.2"

## How it Works ##

The best way to get understanding of how Formaggio works is to have a play on the command line: 
 
     sbt 'project core' console  
     scala> import formaggio.SampleData.{personForm, sampleEnv}
     import formaggio.SampleData.{personForm, sampleEnv}
     scala> sampleEnv
     res0: scala.collection.immutable.Map[java.lang.String,java.lang.String] = Map(age::3 -> 30, name::2 -> Bob, password::7 -> password, name::1 -> Jim, nickname::5 -> Jimbo, password::6 -> password)
     scala> runFormState(personForm, sampleEnv)
     res1: (formaggio.Formlets.ValidForm[formaggio.SampleData.Person], scala.xml.NodeSeq) = (Failure(NonEmptyList((terms::8,GenericError(<function1>)))),NodeSeq(<h1>Errors...
     
The last result is a form failure because the terms and conditions field is not present in the environmont: ie it hasn't been filled in.  To fill it in we can just add to the sampleEnv map:
     
     scala> runFormState(personForm, sampleEnv + ("terms::8" -> "true"))
     res2: (formaggio.Formlets.ValidForm[formaggio.SampleData.Person], scala.xml.NodeSeq) = (Success(Person(FullName(Jim,Bob),30,false,Some(Jimbo),password,Favourites(GreenEggs,List()))),NodeSeq(, <label for="name:...

This form now succeeds.  As you can see when a form succeeds you get a Success object which contains the datatype
that the form produces as well as the filled in form html.  If it fails you get a Failure object which contains a list of entries that have failed as well as the form html along with an html representation of the errrors that have been produced.

To see how to actually go about making forms have a look at core/src/main/scala/scormlets/SampleData.scala to see how personForm is constructed.

## TODO: ##

 * handle file uploads
 * handle radio, multiple checkboxes, drop down selects
 * recaptchas?
 * create examples using lift framework
 * support for different backends eg scalate, commandline
 * i18n support for error messages, field names, etc 
 
## THANKS: ##

  * To Runar Bjarnason for the name.
  
 

