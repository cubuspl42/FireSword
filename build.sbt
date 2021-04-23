enablePlugins(ScalaJSPlugin)

name := "FireSword"
scalaVersion := "2.13.4"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
