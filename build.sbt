enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

name := "FireSword"
scalaVersion := "2.13.4"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
libraryDependencies += "com.github.japgolly.scalacss" %%% "core" % "0.7.0"

Compile / npmDependencies ++= Seq(
  "pako" -> "^1.0.1",
  "@types/pako" -> "^1.0.1",
)
