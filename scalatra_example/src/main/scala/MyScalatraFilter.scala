package com.example

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport
import scala.xml.{Text, Node, NodeSeq}
//import org.apache.commons.io.IOUtils
//import fileupload.FileUploadSupport

import formaggio._
import scalaz._

class MyScalatraFilter extends ScalatraFilter with ScalateSupport {
  import SampleData._
  import Scalaz._
  import Formlets._

  val massinputJsFile = "massinput.js"

  
  get ("/" + massinputJsFile) {
    jsMassInputCode
  }

  def template(x: NodeSeq, title: String = "") = 
    <html>
      <head>
	<title>Scormlets Examples :: { title }</title>
	<script type="text/javascript" src={ jqueryUrl }/>
	<script type="text/javascript" src={ massinputJsFile }/>
      </head>
      <body><h1>{ title }</h1>{ x }</body>
    </html>

  get("/") {
    <html>
      <body>
	<ul>
	  <li>Register <a href="registration">here</a>.</li>
	  <li><a href="massinput">mass input</a></li>
	</ul>
      </body>
    </html>
  }

  def formTemplate(n: NodeSeq) = 
    <form method='POST'>{ n }<input type='submit'/></form> 


  get ("/registration") {
    <html>
      <body>
        <h1>Registration</h1>{
	  formTemplate(getFormView(personForm))
	}
      </body>
    </html>
  }

  post ("/registration") {
    val rslt: NodeSeq = 
      getFormValidation(personForm, params.iterator.toMap) match {
	case Success(s) => 
	  <h3>Congratulations!</h3>
	  <p>You've registered: { s.toString }</p>
	case Failure(v) => <h2>Params: { params }</h2> ++ formTemplate(v)
      }
    // Registration
    ///    <h1>Registration</h1> 
    template(rslt, "Registration")
  }

  get ("/massinput") {
    template(formTemplate(getFormView(favouriteThings)), "Mass Input")
  }

  post ("/massinput") {
    val rslt =
      getFormValidation( favouriteThings, params.iterator.toMap).fold(
	e => <h2>Params: { params }</h2> ++ formTemplate(e),
	s => <p>Mass input result: { s.toString }</p>)
    template(rslt, "Mass Input Result")
  }

}
