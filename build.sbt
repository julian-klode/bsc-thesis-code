import scalariform.formatter.preferences._

scalariformSettings

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, false)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)

scalaSource in Compile := baseDirectory.value

scalaSource in Test := baseDirectory.value


scalaVersion := "2.11.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

initialCommands := Seq(
  "import LIGD._",
  "import LIGDCompany._",
  "import CompanyData._",
  "import EMGM._",
  "import shapeless._"
).mkString(";")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0",
  "com.chuusai" %% "shapeless" % "2.0.0"
)
