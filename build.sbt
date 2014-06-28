import scalariform.formatter.preferences._

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)

scalaSource in Compile := baseDirectory.value

scalaSource in Test := baseDirectory.value


scalaVersion := "2.11.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0"
