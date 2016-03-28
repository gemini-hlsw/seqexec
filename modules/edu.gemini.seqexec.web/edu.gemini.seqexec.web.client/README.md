# Seqexec Web Client

This bundle contains a web-based seqexec client. It contains a SPA (Single-page application) which communicates to the backend using Ajax-style calls and websockets

## sbt plugins

The project is done using [Scala.js](http://www.scala-js.org/) to generate JavaScript out of Scala code. Thus, this bundle requires a new plugin

* [scala-js sbt plugin](http://www.scala-js.org/doc/sbt-plugin.html): This plugins brings the support for scala.js compilation into sbt. Using the compiler we can take the scala code and 

## React.js

On top of the basic scala.js support we need some type of front-end framework to work appropriately on the browser environment. There are plenty of frameworks for JavaScript and a few have been ported to Scala.js. The one selected is [react.js](https://github.com/japgolly/scalajs-react) after checking a few of the other options. 

The main reasons for this selection are:

* Availability of documentation
* The fact that the port of [react.js](https://facebook.github.io/react/) is not a simple facade, but it tries to bring the scala FP style into the framework
* Strong preference for using immutable data

## Other Libraries

The project requires some new libraries on the scala side. Here is a summary of them and the reason to use them:

* [react.js](https://facebook.github.io/react/) Already described
* [Diode](https://github.com/ochrons/diode) For data flow and organization of the application
* [uPickle](https://github.com/japgolly/upickle): Used for json serialization. Even though we have Argonaut already, uPickle runs on both sides of the equation. We may decide to use [BooPickle](https://github.com/ochrons/boopickle) instead for better performance
* [ScalaCSS](https://github.com/japgolly/scalacss/) Type safe css
* [Scala.js DOM](https://github.com/scala-js/scala-js-dom) Must have
* [Boostrap](https://getbootstrap.com/) Responsive HTML framework

Interestingly I see no need of jQuery so far

## TODO

* Package the application compressing with fullOptJS
* Load javascript files from webjars
* Authentication
* Testing