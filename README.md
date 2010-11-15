# Scormlets
Scala Formlets

From the [Haskell Wiki](http://www.haskell.org/haskellwiki/Formlets): 
"[Formlets](http://groups.inf.ed.ac.uk/links/formlets/) are a way of building HTML 
forms that are type-safe, handle errors, abstract and are easy to combine into 
bigger forms." The key feature of formlets is that they are composable: formlets
can be combined to produce larger forms using applicative functors.

I started writing this because I was impressed by the Haskell implementations
of the idea: [Formlets](http://hackage.haskell.org/package/formlets) and 
[Digestive-Functors](http://hackage.haskell.org/package/digestive-functors).  
I also thought it would be a
good way to get to know Scalaz which is pretty much essential when trying 
to port ideas from Haskell to Scala.   

Other implementations:

 * Racket: http://docs.racket-lang.org/web-server/formlets.html
 * Common Lisp: http://www.cliki.net/admin/edit/edit/Formlets

TODO:
I am still learning to get to grips with Scalaz so I'm sure there are many areas
in which this package can be improved:

 * look at using ReaderT for the environment
 * handle file uploads
 * make this work easily with common Scala web frameworks such as Lift and Scalatra
 * support for different backends eg scalate
 
 
 
