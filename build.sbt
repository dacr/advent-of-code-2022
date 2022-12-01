name := "advent-of-code-2022"

version := "0.1"

scalaVersion := "3.2.1"

lazy val versions = new {
  val zio = "2.0.4"
}

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"            % versions.zio,
  "dev.zio" %% "zio-test"       % versions.zio,
  "dev.zio" %% "zio-streams"    % versions.zio,
  "dev.zio" %% "zio-test-junit" % versions.zio % Test,
  "dev.zio" %% "zio-test-sbt"   % versions.zio % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

