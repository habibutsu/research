name := "hibernate"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.mchange" % "c3p0" % "0.9.5.1",
  "org.postgresql" % "postgresql" % "9.4-1202-jdbc42",
  "org.hibernate" % "hibernate-c3p0" %  "5.1.0.Final",
  "org.hibernate" % "hibernate-entitymanager" % "5.1.0.Final"
)
