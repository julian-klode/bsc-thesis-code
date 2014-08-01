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

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0",
  "com.chuusai" %% "shapeless" % "2.0.0"
)
