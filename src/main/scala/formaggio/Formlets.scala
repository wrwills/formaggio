package formaggio

import scalaz._

trait Formlets

/**
 * Formlets
 */
object Formlets extends Html with MassInput {

  import Scalaz._

  import scala.xml.NodeSeq
  import scala.xml.Text
  import Applicative._
  import Pointed._

  // allows us to treat exceptions arising from eg parsing errors as form validation
  // errors  
  //implicit 
  def ExceptionTo(e: Exception): FormException = new FormException {
    val value = e
  }

  def addIndexToValidation[A](i: String, v: Validation[FormError, A]): Validation[(String, FormError), A] =
    v match {
      case Success(s) => s.success
      case Failure(f) => failure(i, f)
    }

  def liftExceptionValidation[A](v: Validation[Exception, A]): Validation[(String, Exception), A] =
    v match {
      case Success(s) => s.success[(String, Exception)]
      case Failure(f) => failure[(String, Exception), A]("", f)
    }

  type Errors = Map[String, String]
  type Env = Map[String, String] // worry about files later
  //type Env = Map[String, NonEmptyList[String]] // worry about files later
  type ValidForm[A] = Validation[NonEmptyList[(String, FormError)], A]

  /*
   * form state is an integer showing the number of the last input together with
   * a list of inputs
   */
  type FormState = (Int, List[String])

  /*
   * A view represents a visual representation of a form. It is a
   * function which takes a list of all errors and then produces a new view
   */
  type View = Errors => NodeSeq

  //trait Form[A] extends NewType[ Env => State[FormState,(ValidForm[A],View)] ] {
  trait Form[A] {
    val default: Option[A] // = None

    val value: Option[A] => Env => State[FormState, (ValidForm[A], View)]

    def run(e: Env) = value(default)(e)

    /*
     * Append a unit form to the left. This is useful for adding labels or error
     * fields
     * only works if current form returns Unit
     * Evaluate the form that matters first, so we have a correct range set 
     */
    def ++>[B](frm: Form[B]): Form[B] =
      Form(
        (dflt: Option[B]) =>
          (env: Env) =>
            for {
              frslt <- frm.value(frm.default)(env)
              s <- init[FormState]
              arslt <- this.value(default)(env)
            } yield (frslt._1, arslt._2 ⊹ frslt._2))

    /*
     * Append a unit form to the right. See '++>'.
     */
    def <++(frm: Form[Unit]): Form[A] =
      Form(
        (dflt: Option[A]) =>
          (env: Env) =>
            for {
              s <- init[FormState]
              arslt <- this.value(dflt)(env)
              frslt <- frm.value(frm.default)(env)
            } yield (arslt._1, arslt._2 ⊹ frslt._2))

    // transform the xml component
    def plug(transform: NodeSeq => NodeSeq): Form[A] =
      Form(
        default,
        (dflt: Option[A]) =>
          (env: Env) =>
            for {
              s <- init[FormState]
              arslt <- this.value(dflt)(env)
            } yield (arslt._1,
              (err: Errors) => transform(arslt._2(err))))

    def setDefault(dflt: A) = Form(dflt, this.value)

    def setDefault(dflt: Option[A]): Form[A] =
      new Form[A] {
        val value: Option[A] => Env => State[FormState, (ValidForm[A], View)] = this.value
        val default = dflt
      }

  }

  object Form {
    def apply[A](fn: Option[A] => Env => State[FormState, (ValidForm[A], View)]) =
      new Form[A] {
        val default = None
        val value = fn
      }

    def apply[A](dflt: A, fn: Option[A] => Env => State[FormState, (ValidForm[A], View)]) =
      new Form[A] {
        val default = Some(dflt)
        val value = fn
      }

    def apply[A](dflt: Option[A], fn: Option[A] => Env => State[FormState, (ValidForm[A], View)]) =
      new Form[A] {
        val default = dflt
        val value = fn
      }
  }

  implicit def FormPure: Pure[Form] = new Pure[Form] {
    def pure[A](a: => A) =
      Form((_: Option[A]) => (env: Env) =>
        for { s <- init[FormState] }
          yield (a.success, (errors: Map[String, String]) => Text("")))
  }

  implicit def FormFunctor: Functor[Form] = new Functor[Form] {
    def fmap[A, B](r: Form[A], f: A => B): Form[B] =
      Form((dflt: Option[B]) => (env: Env) =>
        for { rslt <- r.value(r.default)(env) }
          yield (rslt._1 map f, rslt._2))
  }

  implicit def FormApply: Apply[Form] = new Apply[Form] {

    def apply[A, B](f: Form[A => B], a: Form[A]): Form[B] =
      Form(
        (dflt: Option[B]) =>
          (env: Env) =>
            for {
              frslt <- f.value(f.default)(env)
              arslt <- a.value(a.default)(env)
            } yield (arslt._1 <*> frslt._1, frslt._2 ⊹ arslt._2))
  }

  def liftValidation[A](form: Form[Validation[FormError, A]]): Form[A] =
    validate(form, (x: FormError) => x)

  def validate[A](form: Form[Validation[String, A]]): Form[A] =
    validate(form, (x: String) => GenericError((_: String) => x))

  /**
   * convert a form which returns a validation of A into a form with returns A
   * ie lift the validation of A into the form validation
   */
  def validate[A, B](form: Form[Validation[B, A]], convertFailure: B => FormError): Form[A] =
    Form(
      (dflt: Option[A]) =>
        (env: Env) =>
          for {
            frslt <- form.value(form.default)(env)
            s <- init[FormState]
          } yield (
            frslt._1.fold(
              _.fail[A],
              _.fail.map(
                f => (s._2.headOption.getOrElse("unknown"), convertFailure(f))).validation.liftFailNel),
              frslt._2))

  def convertErrorToErrorMessage(x: (String, FormError)) = (x._1, x._2.getErrorMessage(x._1))

  /*
  trait OptionalForm[A, B <: Option[A]] extends Form[Option[A]] {
//    def nonEmptyo = formNonEmpty(this)
    def ne = this map ( (x:B) => x.toSuccess(EmptyError) )
    //def nonEmptyo: Form[A] = liftValidation( this map (nonEmpty _) )
  }*/

  def nonEmpty[A](x: Option[A]) = x.toSuccess(EmptyError)

  /**
   * run a form within an environment
   */
  def runFormState[A](frm: Form[A], env: Env, showErrors: Boolean = true) = {
    val (valid, view) = frm.value(frm.default)(env) ! (0, List[String]())
    val errors: Errors =
      if (showErrors)
        valid.fail.map(
          _.list.map(convertErrorToErrorMessage(_)).toMap).toOption.getOrElse(Map())
      else
        Map()
    (valid, view(errors))
  }

  /**
   * get the view the form without any errors displayed
   */
  def getFormView[A](frm: Form[A]) =
    runFormState(frm, Map(), false)._2

  /**
   * return a validation showing either the xml for the view or the result
   */
  def getFormValidation[A](frm: Form[A], env: Env): Validation[NodeSeq, A] = {
    val (valid, view) = runFormState(frm, env)
    valid.fail.map(_ => view).validation
  }

  /**
   * like getFormValidation but returns an Either for callers who do not
   * want to import scalaz to use  this library
   */
  def getFormEither[A](frm: Form[A], env: Env) =
    getFormValidation(frm, env).either

}

