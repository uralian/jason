
import xerial.sbt.Sonatype._

publishMavenStyle := true

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

sonatypeProjectHosting := Some(GitHubHosting("uralian", "jason", "vlad@uralian.com"))
