ThisBuild / version := "1.0"
//ThisBuild / scalaVersion := "2.12.16"
//ThisBuild / scalaVersion := "2.13"
//ThisBuild / scalaVersion := "2.13.12"
//ThisBuild / scalaVersion := "2.12.12"
ThisBuild / scalaVersion := "2.12.15"
ThisBuild / organization := "org.example"

//val spinalVersion = "1.9.3"
val spinalVersion = "dev"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
//val libcheesevoyage = "com.github.fl4shk" %% "libcheesevoyage"
//val depVersion = "hash/branch/testVga"
//val depVersion = "testVga"

//lazy val libcheesevoyage = "com.github.fl4shk" %% "libcheesevoyage" % "main" % "testVga"
//val libcheesevoyage = "com.github.fl4shk" % "libcheesevoyage" % "hash" % "tag" % "testVga"
//val libcheesevoyage = "com.github.fl4shk" % "libcheesevoyage" % "hash" % "tag" % "testVga"

//val depVersion = "hash/tag/testVga"
//val depVersion = "tag/testVga"
//val depVersion = "testVga"
//lazy val commonSettings = Seq(
//  //organization := "com.example",
//  //version := "0.1.0",
//  //scalaVersion := "2.11.8"
//  scalacOptions ++= Seq(
//    "-Ybackend-parallelism", "4",
//    "-Ybackend-worker-queue", "4",
//    "-P:semanticdb:sourceroot:."
//  )
//)

// BEGIN: Stuff for larger projects
//val depVersion = "2a16a9ee912e627d3ccaea02e7ecc95ac1193b83"
//lazy val libcheesevoyage = RootProject(uri("https://github.com/fl4shk/libcheesevoyage.git#%s".format(depVersion)))
//libcheesevoyage / scalacOptions ++= Seq(
//  "-Ybackend-parallelism", "4",
//  "-Ybackend-worker-queue", "4",
//  "-P:semanticdb:sourceroot:."
//)
// END: Stuff for larger projects

//commonSettings.map { s => s.mapKey(Def.mapScope(_.in(libcheesevoyage))) }
//commonSettings.map { s => s.mapKey(Def.mapScope(libcheesevoyage / in)) }
//  .settings(
//    scalacOptions ++= Seq(
//        "-Ybackend-parallelism", "4",
//        "-Ybackend-worker-queue", "4",
//        "-P:semanticdb:sourceroot:."
//    )
//  )



//lazy val libcheesevoyageGit = RootProject(uri("https://github.com/fl4shk/libcheesevoyage.git"))
//lazy val libcheesevoyage = RootProject(uri("https://github.com/fl4shk/libcheesevoyage.git"))

//lazy val libcheesevoyage = ProjectRef(uri("https://github.com/fl4shk/libcheesevoyage.git#%s".format(depVersion)))
//lazy val libcheesevoyage = ProjectRef(file("submodules/libcheesevoyage"), "libcheesevoyage")

lazy val libcheesevoyage = (project in file("./submodules/libcheesevoyage"))
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )
//lazy val libcheesevoyage = project in file("submodules/libcheesevoyage")

//lazy val libcheesevoyage = (project in file("./submodules/libcheesevoyage"))
//    .settings(
//        libraryDependencies ++= Seq(
//            spinalCore,
//            spinalLib,
//            spinalIdslPlugin
//        )
//    )
//    .dependsOn(flare32_cpu)
//lazy val libcheesevoyage = (project in file("."))
//  .settings(
//      Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
//  )
//lazy val libcheesevoyage = (project in libcheesevoyageGit)
//  .settings(
//      Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
//  )
//scalacOptions += "-P:semanticdb:sourceroot:/media/other_data/fl4shk_home_stuff/Documents/prog/electronics/spinalhdl/small_projects_and_tests/flare32_cpu"
//scalacOptions += "-Ybackend-parallelism 4"

lazy val flare32_cpu = (project in file("."))
  //.aggregate(libcheesevoyage)
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(
        //libcheesevoyage,
        spinalCore,
        spinalLib,
        spinalIdslPlugin,
    ),
    //scalacOptions ++= Seq(
    //  "-Ybackend-parallelism", "4",
    //  "-Ybackend-worker-queue", "4",

    //)
    scalacOptions ++= Seq(
        "-Ybackend-parallelism", "4",
        "-Ybackend-worker-queue", "4",
        "-P:semanticdb:sourceroot:."
    )

    //libraryDependencies ++= Seq(
    //  libcheesevoyage
    //)
    //libraryDependencies ++= Seq(
    //  libcheesevoyage
    //)
    //libraryDependencies ++= Seq(
    //    libcheesevoyage
    //)

    //libraryDependencies.append(
    //    libcheesevoyage
    //)

  ).dependsOn(libcheesevoyage)
//flare32_cpu.dependsOn(libcheesevoyage % "compile->libcheesevoyage")
//flare32_cpu.dependsOn(libcheesevoyage)

fork := true
