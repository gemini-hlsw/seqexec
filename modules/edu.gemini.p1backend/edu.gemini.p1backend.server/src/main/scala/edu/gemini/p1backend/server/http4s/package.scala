package edu.gemini.p1backend.server

package object http4s {
  def index(devMode: Boolean, builtAtMillis: Long): String = {
    val deps = if (devMode) "edu_gemini_p1backend_client-jsdeps.js" else s"edu_gemini_p1backend_client-jsdeps.min.$builtAtMillis.js"
    val scalajsScript = if (devMode) s"p1backend.js" else s"p1backend-opt.$builtAtMillis.js"
    val xml = <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        <meta name="description" content="Phase1 Backend"/>
        <meta name="author" content="Gemini Software Group"/>
        <link rel="icon" href={s"images/launcher.$builtAtMillis.ico"}/>

        <!-- Add to homescreen for Safari on iOS -->
        <meta name="apple-mobile-web-app-capable" content="yes"/>
        <meta name="apple-mobile-web-app-status-bar-style" content="black"/>
        <meta name="apple-mobile-web-app-title" content="Phase1 Backend"/>
        <link rel="apple-touch-icon-precomposed" href={s"images/launcher.$builtAtMillis.png"}/>

        <title>Phase1 Backend</title>

        <link rel="stylesheet" href={s"css/semantic.$builtAtMillis.css"}/>
      </head>

      <body>

        <div id="content">
        </div>

        <script src={deps}></script>
        <script src={scalajsScript}></script>
        <script type="text/javascript">
          P1BackendApp().main();
        </script>
      </body>
    </html>
    s"<!DOCTYPE html>$xml"
  }

}
