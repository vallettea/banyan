import sbt._
import Keys._
import com.github.retronym.SbtOneJar

object MyBuild extends Build {

    lazy val sharedSettings = Project.defaultSettings ++ Seq(
        name := "Banyan",
        description := "Gradient Boosted decision tree",
        organizationName := "Snips",
        organization := "net.snips",
        version := "0.1.0",
        scalaVersion := "2.10.3",

        libraryDependencies ++= Seq(
            "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
        ),

        resolvers ++= Seq(
            Opts.resolver.sonatypeSnapshots,
            Opts.resolver.sonatypeReleases,
            "Clojars Repository" at "http://clojars.org/repo",
            "Conjars Repository" at "http://conjars.org/repo"
        ),
                
        scalacOptions ++= Seq(
            "-unchecked",
            "-deprecation",
            "-feature",
            "-language:implicitConversions",
            "-language:reflectiveCalls",
            "-Yresolve-term-conflict:package"
        ),

        publishMavenStyle := true,
        pomIncludeRepository := { x => false },
        publishArtifact in Test := false,
        exportJars := true
        
    ) ++ SbtOneJar.oneJarSettings

  

    lazy val banyan = Project("banyan", file("."), settings = sharedSettings).settings(
        mainClass in (Compile, run) := Some("net.snips.banyan.examples.Laposte"),
        mainClass in "one-jar" := Some("net.snips.banyan.examples.Laposte")
        )

}