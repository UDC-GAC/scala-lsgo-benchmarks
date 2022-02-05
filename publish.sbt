// publishTo target: Sonatype or GitHub Packages

/** sbt-github-packages settings ----------------------------------------------------------------*/
//publishTo := githubPublishTo.value
//githubOwner := "xoanpardo"
//githubRepository := "scala-lsgo-benchmarks"
//githubTokenSource := TokenSource.GitConfig("github.token") // token to publish to github in ~/.gitconfig


/** sbt-sonatype settings -----------------------------------------------------------------------*/
publishTo := sonatypePublishToBundle.value // by default release to Sonatype

sonatypeCredentialHost := "s01.oss.sonatype.org" // For Sonatype accounts created after February 2021

// To sync with Maven central, you need to supply the following information:
publishMavenStyle := true

// Open-source license (already defined in build.sbt)
//licenses := Seq("GPL-3.0-only" -> url("https://www.gnu.org/licenses/gpl-3.0.html"))

// source code hosted in GitHub
import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("xoanpardo", "scala-lsgo-benchmarks", "xoan.pardo@udc.gal"))

developers := List(
  Developer(id="xoanpardo", name="Xoan C. Pardo", email="xoan.pardo@udc.gal", url=url("https://pdi.udc.es/en/File/Pdi/MF5AF"))
)