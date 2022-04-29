crossScalaVersions := Seq("2.12.15", "2.13.8")

organization := "io.github.xoanpardo"
name := "scala-lsgo-benchmarks"
version := "0.1.2"
scalaVersion := "2.13.8"

// IntelliJ IDEA flags
idePackagePrefix := Some("gal.udc.gac.lsgo2013")

// sbt-header settings
organizationName := "Xoan C. Pardo"
startYear := Some(2022)
licenses += ("GPL-3.0-only", new URL("https://www.gnu.org/licenses/gpl-3.0.html"))

// dependencies
libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.3" % "provided",
  // "org.scalactic" %% "scalactic" % "3.2.10",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)