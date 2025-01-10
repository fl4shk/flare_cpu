ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.17"
//ThisBuild / scalaVersion := "2.13"
//ThisBuild / scalaVersion := "2.13.12"
//ThisBuild / scalaVersion := "2.12.12"
//ThisBuild / scalaVersion := "2.12.15"
ThisBuild / organization := "org.example"

val spinalVersion = (
  //"1.9.3"
  //"dev"
  //"1.10.0"
  //"1.10.1"
  "1.10.2a"
)
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

// BEGIN: Stuff for larger projects
//val depVersion = "2a16a9ee912e627d3ccaea02e7ecc95ac1193b83"
//lazy val libcheesevoyage = RootProject(uri("https://github.com/fl4shk/libcheesevoyage.git#%s".format(depVersion)))
//libcheesevoyage / scalacOptions ++= Seq(
//  "-Ybackend-parallelism", "4",
//  "-Ybackend-worker-queue", "4",
//  "-P:semanticdb:sourceroot:."
//)
// END: Stuff for larger projects


lazy val libcheesevoyage = (
  project in file("./submodules/libsnowhouse/submodules/libcheesevoyage")
)
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )
lazy val libsnowhouse = (project in file("./submodules/libsnowhouse"))
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )

lazy val flare_cpu = (project in file("."))
  //.aggregate(libcheesevoyage)
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(
        //libcheesevoyage,
        spinalCore,
        spinalLib,
        spinalIdslPlugin,
    ),
    scalacOptions ++= Seq(
        "-Ybackend-parallelism", "4",
        "-Ybackend-worker-queue", "4",
        "-P:semanticdb:sourceroot:."
    )


  ).dependsOn(libsnowhouse, libcheesevoyage)

fork := true
