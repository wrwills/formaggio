package formaggio

import scalaz._

trait MassInput {
  import Scalaz._
  import Formlets._
  import scala.xml._

  /**
   * a formlet for inputting lists of things
   * : Form[Seq[A]] =
   */
  def massInput[A](formlet: Form[A], default: Seq[A] = Seq(),
    itemWrapper: (NodeSeq => NodeSeq) = (x: NodeSeq) => <li class="massInputItem">{ x }</li>,
    listWrapper: (NodeSeq => NodeSeq) = (x: NodeSeq) => <ul>{ x }</ul>): Form[Seq[A]] = {
    val pluggedFormlet: Form[A] = formlet.plug(itemWrapper)
    // pluggedFormlet.setDefault()

    def getDefault(i: Int) =
      if (default.size < i) Some(default(i)) else None

    val frm: Form[Seq[A]] =
      Form(
        default,
        (dflt: Option[Seq[A]]) =>
          (env: Env) =>
            for {
              s <- init[FormState];
              val ns = ((s._1 + 1) * 100, List[String]());
              _ <- put(ns);
              val mI: Seq[Form[A]] = if (env.size == 0) {
                println("defaults" + default + dflt)
                val defaultViews = default map (d => { 
                  println("default" + d + ":: " + ns)
                  val fl = formlet.setDefault(Some(d))
                  println(fl)
                  //getFormView(formlet.setDefault(Some(d)))
                  //val f = fl.value(Some(d))(Map())(ns)
                  val f = formlet.value(Some(d))(Map())(ns)
                  //val f = pluggedFormlet.setDefault(d).value(Some(d))(Map())(ns)
                  f._2._2(Map())
                })

                println("defaultviews: " + defaultViews)
                val dflts = default map (pluggedFormlet.setDefault(_))                
                dflts
              } else {
                val lngth: Int = checkEnvironmentForMassInput(formlet, env, ns)
                ((0 until lngth) map (_ => pluggedFormlet))
                //((0 until lngth) map (i => pluggedFormlet.setDefault(getDefault(i)))) sequence
                // val mI = ((0 until lngth) map (i => pluggedFormlet.setDefault(getDefault(i)))) sequence
              }
              /*
            val mI = ((0 until lngth) map (i => 
              formlet.setDefault(getDefault(i)).plug(itemWrapper)
                                         )) sequence
                                         */
              // val mI = ((0 until lngth) map (_ => pluggedFormlet)) sequence;              

              // run the form in the environment and add the errors from that form to
              // the form state
              val mi2: Form[Seq[A]] = mI sequence              
              //val frmN = mi2.value(None)(env)(ns);
              val frmN = mi2.value(dflt)(env)(ns)
              
              nns <- modify((x: FormState) => (s._1 + 1, x._2 ++ s._2 ++ frmN._1._2))
            } yield {
              val flts = dflt.flatten              
              val frm = frmN._2
              // we add another bit of html to the form but don't include it as part of what 
              // gets validated
              val frmView = (pluggedFormlet.value(pluggedFormlet.default)(Map()) ! frmN._1)._2
              (frm._1, frm._2 ⊹ frmView)
            })
      
      frm
    //frm.plug(listWrapper andThen buttons _ andThen wrapperDiv _)
  }

  //def nonEmptyMassInput[A](mI: Form[Seq[A], 

  /*
   * Find the number of mass input items by recursively running through state until we
   * get to an input name which is not present in the form environment
   */
  def checkEnvironmentForMassInput[A](formlet: Form[A], env: Env, state: FormState): Int = {
    val nState = formlet.value(formlet.default)(env)(state)
    nState._2._1.fail.map(_.list.filter(_._2 == LookupError).headOption).validation match {
      case Failure(Some(_)) => 0
      case _ => 1 + checkEnvironmentForMassInput(formlet, env, nState._1)
    }
  }

  def wrapperDiv(x: NodeSeq) = <div class="massInput">{ x }</div>

  // this needs to be imported into any page which uses mass input				    
  val jqueryUrl = "http://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"

  /*
  buttons x = (X.thediv
  ((X.input ! [X.thetype "button", X.strAttr "onclick" "addItem(this); return false;", X.value "Add Item"]) +++
  (X.input ! [X.thetype "button", X.strAttr "onclick" "removeItem(this); return false;", X.value "Remove Last Item"]))) +++ x */
  def buttons(x: NodeSeq) =
    <div>
      <input type="button" onclick="addItem(this); return false;" value="Add Item"/>
      <input type="button" onclick="removeItem(this); return false;" value="Remove Last Item"/>{ x }
    </div>

  /*
   * javascript for mass input functionality
   * - use jquery to get the last massinput item
   * - get the last state number for that item
   * - create a copy of that item incrementing state for every input name attribute
   */
  val jsMassInputCode = """
function findItems(button) {
  var mainDiv = $(button).parent();
  while ( !mainDiv.hasClass('massInput') ) {
    mainDiv = $(mainDiv).parent();
  }
  return $('.massInputItem', mainDiv);
}

function incState(str, p1, offset, s) { var sp = str.split("::"); state = state + 1; return sp[0] + "::" + state; }

function setState(str, p1, offset, s) { var sp = str.split("::"); state = parseInt(sp[1]); return state; }

function addItem(button) {
  var items = findItems(button);
  var item = $(items[items.length-1]);
  var newItem = item.clone(true);

  var state = 0;
  //var regExp = /::\d\d\d/g;
  var regExp = /name=\".*::\d\d\d/g
  
  newItem.html().replace(regExp,setState);
  newItem.html(newItem.html().replace(regExp,incState))
  
  newItem.children('input').attr('value','');
  newItem.appendTo(item.parent());
}
function removeItem(button) {
  var items = findItems(button);
  if ( items.length > 1 ) {
    var item = $(items[items.length-1]);
    item.remove();
  } else {
    alert('Cannot remove any more rows');
  }
}
"""

}
