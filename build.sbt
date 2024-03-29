val dottyVersion = "3.1.0"

scalaVersion := dottyVersion

val baseAquaVersion = settingKey[String]("base aqua version")

val catsV = "2.7.0"
val catsParseV = "0.3.6"
val monocleV = "3.0.0-M6"
val scalaTestV = "3.2.10"
val fs2V = "3.2.3"
val catsEffectV = "3.3.1"
val declineV = "2.2.0"
val circeVersion = "0.14.1"
val scribeV = "3.6.3"

name := "aqua-hll"

val commons = Seq(
  baseAquaVersion := "0.5.3",
  version         := baseAquaVersion.value + "-" + sys.env.getOrElse("BUILD_NUMBER", "SNAPSHOT"),
  scalaVersion    := dottyVersion,
  libraryDependencies ++= Seq(
    "com.outr"      %%% "scribe"    % scribeV,
    "org.scalatest" %%% "scalatest" % scalaTestV % Test
  ),
  scalacOptions ++= {
    Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:implicitConversions",
      "-unchecked",
      "-Ykind-projector"
      //      "-Xfatal-warnings"
    )
  }
)

commons

lazy val cli = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("cli"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect"    % catsEffectV,
      "com.monovore"  %%% "decline"        % declineV,
      "com.monovore"  %%% "decline-effect" % declineV,
      "co.fs2"        %%% "fs2-io"         % fs2V
    )
  )
  .dependsOn(compiler, `backend-air`, `backend-ts`)

lazy val cliJS = cli.js
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true
  )

lazy val cliJVM = cli.jvm
  .settings(
    Compile / run / mainClass  := Some("aqua.AquaCli"),
    assembly / mainClass       := Some("aqua.AquaCli"),
    assembly / assemblyJarName := "aqua-" + version.value + ".jar",
    libraryDependencies ++= Seq(
    )
  )

lazy val types = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )

lazy val parser = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % catsParseV,
      "org.typelevel" %%% "cats-free"  % catsV
    )
  )
  .dependsOn(types)

lazy val linker = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .dependsOn(parser)

lazy val raw = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/raw"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % catsV
    )
  )
  .dependsOn(types)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % catsV
    )
  )
  .dependsOn(types, raw)

lazy val transform = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/transform"))
  .settings(commons: _*)
  .dependsOn(model, raw)

lazy val `test-kit` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/test-kit"))
  .settings(commons: _*)
  .dependsOn(transform)

lazy val semantics = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %%% "monocle-core"  % monocleV,
      "com.github.julien-truffaut" %%% "monocle-macro" % monocleV
    )
  )
  .dependsOn(raw, `test-kit` % Test, parser)

lazy val compiler = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("compiler"))
  .settings(commons: _*)
  .dependsOn(semantics, linker, backend)

lazy val backend = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend"))
  .settings(commons: _*)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "aqua.backend"
  )
  .dependsOn(transform)

lazy val `backend-air` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/air"))
  .settings(commons: _*)
  .dependsOn(backend)

lazy val `backend-ts` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/ts"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(`backend-air`)
