ThisBuild / organization := "org.readren.json-facile"
ThisBuild / version      := "0.2.1-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / autoAPIMappings := true
// ThisBuild / apiURL := Some(url(s"file:${(baseDirectory).value.getAbsolutePath}/target/scala-${scalaVersion.value.take(4)}/api"))

lazy val akkaHttpVersion = "10.2.0"

lazy val jsfacile = (project in file("."))
	.aggregate(core, macros, comun)
	.settings(
		publish / skip := true,
	)

lazy val core = (project in file("core"))
	.dependsOn(
		macros,
		comun % "compile-internal, test-internal" // the "compile-internal" removes `common` from the set of dependencies for publishing because its content is provided by the artifact of this package. See the mappings below. Also see "https://www.scala-sbt.org/1.x/docs/Macro-Projects.html"
	) 
	.settings(
		// append the content of the common binary package to the core binary package
		Compile / packageBin / mappings ++= (comun / Compile / packageBin / mappings).value,
		// append the content of the common source package to the core source package 
		Compile / packageSrc / mappings ++= (comun / Compile / packageSrc / mappings).value,
		// append the content of the common javadoc package to the core javadoc package. This is convenient and also necessary for the doc links between projects to work. 
		Compile / doc / sources ++= (comun / Compile / doc / sources).value,
		// append the content of the macros javadoc package to the core javadoc package. This is convenient and also necessary for the doc links between projects to work. 
		Compile / doc / sources ++= (macros / Compile / doc / sources).value,
	)

lazy val macros = (project in file("macros")).dependsOn(comun % "compile-internal, test-internal") // the "compile-internal" removes `common` from the set of dependencies for publishing because it is provided by the core artifact.
	.settings(
		// the content of the macros project's javadoc package is appended to the core project's javadoc package. This setting avoids to publish the javadoc of this project.
		Compile / packageDoc / publishArtifact := false,
	)

lazy val comun = (project in file("comun"))
	.settings(
		name := "common",
		publish / skip := true,
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
	// scala reflection required for macros and annotations
	"org.scala-lang" % "scala-reflect" % scalaVersion.value,
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


/** Bloop settings */

// Source dependencies are not well supported in sbt. This setting allows to use them and generates bloop configuration files for them too.
bloopAggregateSourceDependencies in Global := true
// This setting enables source classifiers and downloads the sources of binary dependencies. Required if you are using bloop with IDEs (e.g. Metals or IntelliJ) and expect navigation to binary dependencies to work.
bloopExportJarClassifiers in Global := Some(Set("sources"))



