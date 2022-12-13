//enablePlugins(ScalaNativePlugin)

name := "advent-of-code-2022"

version := "0.1"

scalaVersion := "3.2.1"

lazy val versions = new {
  val zio = "2.0.5"
  val nio = "2.0.0"
}

libraryDependencies ++= Seq(
  "dev.zio"                %% "zio"                      % versions.zio,
  "dev.zio"                %% "zio-nio"                  % versions.nio,
  "dev.zio"                %% "zio-test"                 % versions.zio,
  "dev.zio"                %% "zio-streams"              % versions.zio,
  "dev.zio"                %% "zio-test"                 % versions.zio % Test,
  "dev.zio"                %% "zio-test-sbt"             % versions.zio % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
  // "dev.zio" %% "zio-test-junit" % versions.zio % Test
)

//libraryDependencies += "org.scala-native" %%% "junit-runtime" % "0.4.9"
//testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v")

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
