val Http4sVersion  = "0.18.19"
val Specs2Version  = "4.1.0"
val LogbackVersion = "1.2.3"
val circeVersion = "0.10.0"
lazy val doobieVersion = "0.4.2"

//val itvOasvcLibs = "ITV Oasvc repo" at "http://itvrepos.jfrog.io/itvrepos/oasvc-ivy"
//val credentialsLocation: RichFile =
//  sys.props.get("credentials.location").map(Path(_)).getOrElse(Path.userHome / ".ivy2" / ".credentials")

lazy val root = (project in file("."))
  .settings(
    organization := "com.example",
    name := "checkout",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.7",
    libraryDependencies ++= Seq(
      "org.scalatest"    %% "scalatest"           % "3.0.5" % "test",
      "org.http4s"       %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"       %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"       %% "http4s-circe"        % Http4sVersion,
      "org.http4s"       %% "http4s-dsl"          % Http4sVersion,
      "org.specs2"       %% "specs2-core"         % Specs2Version % "test",
      "ch.qos.logback"   % "logback-classic"      % LogbackVersion,
      "org.slf4j"        % "slf4j-api"            % "1.7.25",
      "com.github.cb372" %% "scalacache-core"     % "0.24.0",
      "com.github.cb372" %% "scalacache-caffeine" % "0.24.0",
      "io.circe" %% "circe-core"                  % circeVersion,
      "io.circe" %% "circe-generic"               % circeVersion,
      "io.circe" %% "circe-parser"                % circeVersion,
      "io.circe" %% "circe-literal"               % "0.10.0-M1",
      "org.tpolecat" %% "doobie-core"       % doobieVersion,
      "org.tpolecat" %% "doobie-postgres"   % doobieVersion,
      "org.tpolecat" %% "doobie-specs2"     % doobieVersion,
      "com.github.seratch" %% "awscala-sqs" % "0.8.+"
    ),
    
    addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.6"),

       addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4"),
//    resolvers += itvOasvcLibs,
//    credentials += Credentials(credentialsLocation.asFile)


  )
