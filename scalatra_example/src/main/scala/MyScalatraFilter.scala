package com.example

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport
import scala.xml.{Text, Node}
//import org.apache.commons.io.IOUtils
//import fileupload.FileUploadSupport

import scormlets._


class MyScalatraFilter extends ScalatraFilter with ScalateSupport {
  import SampleData._
  import Formlets._

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.
      </body>
    </html>
  }

//	  <form action={url("/registration")} method='POST'>{
  get ("/registration") {
    <html>
      <body>
        <h1>Registration</h1> 
	  <form method='POST'>{
	  getFormView(personForm)
	  }
    <input type='submit'/>
    </form></body>
    </html>
  }

  post ("/registration") {
    <html>
      <body>
        <h1>Registration</h1> {
	  params
	}    
    </body>
    </html>
  }

  /*
  notFound {
    // If no route matches, then try to render a Scaml template
    val templateBase = requestPath match {
      case s if s.endsWith("/") => s + "index"
      case s => s
    }
    val templatePath = "/WEB-INF/scalate/templates/" + templateBase + ".scaml"
    servletContext.getResource(templatePath) match {
      case url: URL => 
        contentType = "text/html"
        templateEngine.layout(templatePath)
      case _ => 
        filterChain.doFilter(request, response)
    } 
  }*/
}
