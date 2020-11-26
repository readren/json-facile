ThisBuild / organization := "readren"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.3"

lazy val akkaHttpVersion = "10.2.0"

lazy val commonSettings = Seq(
//   target := { baseDirectory.value / "target2" }
)

lazy val core = (project in file("core")).dependsOn(macros, comun)
	.settings(
		commonSettings,
		// other settings
	)

lazy val macros = (project in file("macros")).dependsOn(comun)
	.settings(
		commonSettings
		// other settings
  	)

lazy val comun = (project in file("comun"))

ThisBuild / libraryDependencies ++= Seq(
	// test
	"org.scalatest" %% "scalatest" % "3.2.2" % Test,
	"org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test
)
core / libraryDependencies ++= Seq(
	// spray json used for testing
	"com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion % Test
)
macros / libraryDependencies ++= Seq(
	// scala reflection required for macros
	"org.scala-lang" % "scala-reflect" % scalaVersion.value
)

ThisBuild / scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-opt:l:method", // Enables intra-method optimizations: unreachable-code,simplify-jumps,compact-locals,copy-propagation,redundant-casts,box-unbox,nullness-tracking,closure-invocations,allow-skip-core-module-init,assume-modules-non-null,allow-skip-class-loading.
	"-language:higherKinds"/*,
	"-Ymacro-debug-lite",
	"-Xlog-implicits"*/
)
core / scalacOptions += "-language:experimental.macros"
macros / scalacOptions += "-language:experimental.macros"



