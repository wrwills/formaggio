import scalaz._

object Formlets2 {
  import Scalaz._

  import scala.xml.{Node => XNode}
  import scala.xml.NodeSeq

  type Env = Map[String, String] // worry about files later
  /*
  type FormState = List[Int]
  type S[A] = State[FormState,A]
  */

  type ValidForm[A] = Validation[NonEmptyList[String],A]

  //newtype Form xml m a = Form { deform :: Env -> S (m (Validator a), m xml, FormContentType) }
  trait Form[A] extends NewType[ Env => State[NodeSeq,ValidForm[A]]]
  object Form {
    def apply[A](fn: Env => State[NodeSeq,ValidForm[A]]) = 
      new Form[A]{ val value = fn }
  }

  //implicit def FormApplicative[X: Semigroup]: Applicative[PartialApply1Of2[Validation, X]#Apply] = applicative[PartialApply1Of2[Validation, X]#Apply](ValidationPure, ValidationApply)
  
  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) = 
      Form((env: Env) => 
	for {s <- init[NodeSeq]} 
	yield success[NonEmptyList[String],A](a))
  }

  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] = 
      Form((env: Env) => 
	for {s <- init[NodeSeq];
	     _ <- modify((_: NodeSeq) ++ scala.xml.Text(""))
	   }
	yield {
	  val newf: State[NodeSeq,ValidForm[A]] = r(env)
	  //val frm: ValidForm[A] = newf ! scala.xml.Text("")
	  val frm: ValidForm[A] = newf ! scala.xml.Text("")
	  
	  
	  error("undefined")
	})	  
  }
  /*
  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B) = error("undefined")
  }*/
  /*
  implicit def FormApply: Apply[Form] = new Apply[Form] {
    def apply[A,B](f: => Form[A => B], a: => Form[A]): Form[B] = 
      error("undefined")      
  }*/
  /*
      FunctorBindApply[PartialApply1Of2[Form, S]#Apply]
      Form((env: Env) => 
*/	
      
   

      
//    }

  //implicit def FormApplicative: Applicative[Form] = new Functor[Form] {
  

  //instance (Functor m, Monad m) => Functor (Form xml m) where
  //fmap f (Form a) = Form $ \env -> (fmap . fmapFst3 . liftM . liftM . fmap) f (a env)
  /*
  implicit def ValidationPure[X]: Pure[PartialApply1Of2[Validation, X]#Apply] = new Pure[PartialApply1Of2[Validation, X]#Apply] {
    def pure[A](a: => A) = a.success
  }*/

  
  def input[A](name: String, fn: String => A): Form[A] = {
    val lookupName = name // later use statemonad to wrap this up    
    val form: Env => State[NodeSeq,ValidForm[A]] = 
      (env: Env) =>
	(init[NodeSeq] <* modify 
	 ((_:NodeSeq) ++ 
	  <label>{ lookupName }</label><input type="text" name="some-form-f0" id="some-form-f0" value="" class="digestive-input" />)) ∘ 
	{ s: NodeSeq =>        	 
	  (env.get(lookupName).toSuccess(
	    nel("could not lookup for " + name,List())
	  )  ∘ fn)
       }
    Form(form)
  }

  sealed trait Name extends NewType[String]
  object Name {
    def apply(s: String): Validation[String, Name] = 
      if (s.headOption.exists(_.isUpper))
	(new Name {val value = s}).success
      else
	"Name must start with a capital letter".fail
  }

  sealed trait Age extends NewType[Int]
  object Age {
    def apply(a: Int): Validation[String, Age] = 
      if (0 to 130 contains a)
	(new Age {val value = a}).success
      else
	"Age must be in range".fail
  }

  case class Person(name: Name, age: Age)
  case class Test(s: String)
  case class TwoTests(a: Test, b: Test)

  def testForm = input("Name1", Test)

  //def testForm2 = input("Name1", Test) <*> (input("Name2", Test)  ∘ TwoTests.curried)


}
