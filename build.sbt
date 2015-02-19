resolvers ++= (
  if (scalaVersion.value.endsWith("-SNAPSHOT"))
    List(
      "pr-scala snapshots" at "http://private-repo.typesafe.com/typesafe/scala-pr-validation-snapshots/",
      Resolver.sonatypeRepo("snapshots")
    )
  else
    Nil
  )
