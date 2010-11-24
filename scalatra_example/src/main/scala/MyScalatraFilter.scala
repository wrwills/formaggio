package com.example

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport
import scala.xml.{Text, Node, NodeSeq}
//import org.apache.commons.io.IOUtils
//import fileupload.FileUploadSupport

import scormlets._
import scalaz._

class MyScalatraFilter extends ScalatraFilter with ScalateSupport {
  import SampleData._
  import Scalaz._
  import Formlets._

  get("/") {
    <html>
      <body>
        <p>Register <a href="registration">here</a>.</p>
      </body>
    </html>
  }

  def formTemplate(n: NodeSeq) = 
    <form method='POST'>{ n }<input type='submit'/></form> 

//	  <form action={url("/registration")} method='POST'>{
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
    <html>
      <body>
        <h1>Registration</h1> {
	  getFormValidation(personForm, params.iterator.toMap) match {
	    case Success(s) => 
	      <h3>Congratulations!</h3>
	      <p>You've registered: { s.toString }</p>
	    case Failure(v) => formTemplate(v)
	  }
	}    
    </body>
    </html>
  }

}
