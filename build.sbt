ThisBuild / organization := "readren"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / autoAPIMappings := true // for unidoc

lazy val akkaHttpVersion = "10.2.0"

lazy val jsfacile = (project in file("."))
	.aggregate(core, macros, comun)
	.settings(
		publishArtifact := false,
	)

lazy val core = (project in file("core")).dependsOn(macros, comun)
	.settings(
		// append the content of the common binary package to the core binary package
		Compile / packageBin / mappings ++= (comun / Compile / packageBin / mappings).value,
		// append the content of the common source package to the core source package 
		Compile / packageSrc / mappings ++= (comun / Compile / packageSrc / mappings).value,
		// append the content of the common javadoc package to the core doc package 
		Compile / doc / sources ++= (comun / Compile / doc / sources).value,
	)

lazy val macros = (project in file("macros")).dependsOn(comun)
	.settings(
		// other settings
  	)

lazy val comun = (project in file("comun"))
	.settings(
		publishArtifact := false,
	)

ThisBuild / libraryDependencies ++= Seq(
	// test dependencies
	"org.scalatest" %% "scalatest" % "3.2.2" % Test,
	"org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
)
core / libraryDependencies ++= Seq(
	// spray json used for testing and by the speed test
	"com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion % Test,
	// jsoniter used by the speed test
	"com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.6.2" % Test,
  	"com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.2" % Test,
)
macros / libraryDependencies ++= Seq(
	// scala reflection required for macros
	"org.scala-lang" % "scala-reflect" % scalaVersion.value
)
comun / libraryDependencies ++= Seq(
	// scala reflection required for annotations
	"org.scala-lang" % "scala-reflect" % scalaVersion.value
)

ThisBuild / scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-opt:l:method", // Enables intra-method optimizations: unreachable-code,simplify-jumps,compact-locals,copy-propagation,redundant-casts,box-unbox,nullness-tracking,closure-invocations,allow-skip-core-module-init,assume-modules-non-null,allow-skip-class-loading.
	"-language:higherKinds",
	// "-Ymacro-debug-lite",
	// "-Xlog-implicits"
)
core / scalacOptions += "-language:experimental.macros"
macros / scalacOptions += "-language:experimental.macros"


