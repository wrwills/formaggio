import scalaz._
/*
case class Form(val env: Map[String, String]) {
  import Scalaz._
  
  
  
}*/

class Formlets3 {
  import Formlets3._
  import Scalaz._
  import scala.xml.NodeSeq
  import scala.xml.Text

  var serrors: Map[String,String] = Map()

  //import scala.xml.{Node => XNode}

  /*
  trait View extends NewType[ Map[String,String] => NodeSeq]
  object View {
    def apply(fn: Map[String,String] => NodeSeq) = 
      new View{ val value = fn }
  }
  implicit def ViewSemigroup[View] = new Semigroup[View] {
    def append(s1: View, s2: => View) = s1.value.append(s2.value)
  }*/

    /*
  case class View(val fn: Map[String,String] => NodeSeq)
  implicit def ViewSemigroup[View] = new Semigroup[View] {
    def append(s1: View, s2: => View) = s1.fn ⊹ s2.fn
  }

  val testView3 = View((errors: Map[String,String]) => Seq(<test id="id1">{ errors.getOrElse( "id1", "ok") }</test>))
  val testView4 = View((errors: Map[String,String]) => Seq(<test id="id2">{ errors.getOrElse( "id2", "ok") }</test>))
  (testView3 ⊹ testView4).apply(Map("id1"->"you entered the wrong thing"))
  */

  /*
   * trait View[A,V] = NewType[ [(FormRange, A)] -> V]
  
  implicit def ViewZero: Zero[View] = new 
  implicit def ViewSemigroup[View] = semigroup(_ ++ _)
  implicit def ViewMonoid: Monoid[View] = new
  */


  /*
  type S[A] = State[FormState,A]
  */

  //type ValidForm[A] = Validation[NonEmptyList[String],A]

  //def testV(errors: Map[String,String]) = <test/>

  
  def inputText(name: String, default: Option[String]): Form[String] =
    (env: Env) => {
      val nerrors = serrors + (name -> ("Need a value for " + name))
      val rslt =  validation(env.get(name).toRight(nerrors))
      if (rslt.isFailure) { serrors = nerrors }
      val view: View = (errors: Map[String,String]) => Seq(
	<input type="text" name={ name } id={ name } 
	value={ rslt.toOption.getOrElse(default.getOrElse("")) } class="digestive-input" />)
      (rslt, view)
    }      

    

}

object Formlets3 {
  import Scalaz._
  import scala.xml.NodeSeq

  type Env = Map[String, String] // worry about files later
  type FormState = List[Int]
  type Form[A] = Env => (Validation[Map[String,String],A],View)

  /*
   * A view represents a visual representation of a form. It is a
   * function which takes a list of all errors and then produces a new view
   */
  type View = Map[String,String] => NodeSeq


  val testV2 = (errors: Map[String,String]) => <test/>
  //val testView = View( (errors: Map[String,String]) => <test/>)
  val testView1 = (errors: Map[String,String]) => Seq(<test id="id1">{ errors.getOrElse( "id1", "ok") }</test>)
  val testView2 = (errors: Map[String,String]) => Seq(<test id="id2">{ errors.getOrElse( "id2", "ok") }</test>)
  (testView1 ⊹ testView2).apply(Map("id1"->"you entered the wrong thing"))

  testView1 ⊛ testView2

  // problem we need to use append for testView and applicative for form

  val frm = new Formlets3()
  
  val ninput = frm.inputText("name", None)
  val ninput2 = frm.inputText("surname", None)

  //val testForm: Form[Pair[String,String]] = (ninput ⊛ ninput2){Pair(_,_)}
  val testForm = (ninput ⊛ ninput2){Pair(_,_)}
  //val testForm: Form[Pair[String,String]] = (ninput ⊛ ninput2){Pair(_,_)}
}
