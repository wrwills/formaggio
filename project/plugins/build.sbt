resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

//Following means libraryDependencies += "com.github.siasia" %% "xsbt-web-plugin" % <sbt version>
libraryDependencies <+= sbtVersion("com.github.siasia" %% "xsbt-web-plugin" % _)
