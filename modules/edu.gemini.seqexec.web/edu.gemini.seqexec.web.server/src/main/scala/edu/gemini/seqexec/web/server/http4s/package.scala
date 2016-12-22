package edu.gemini.seqexec.web.server

package object http4s {
  def index(devMode: Boolean, builtAtMillis: Long) = {
    val style = """
                  |   @media screen and (-webkit-min-device-pixel-ratio:0) {
                  |        select,
                  |        textarea,
                  |        input {
                  |          font-size: 16px !important;
                  |        }
                  |      }
                  |"""
    val deps = if (devMode) "edu_gemini_seqexec_web_client-jsdeps.js" else s"edu_gemini_seqexec_web_client-jsdeps.min.${builtAtMillis}.js"
    val seqexecScript = if (devMode) s"seqexec.js" else s"seqexec-opt.${builtAtMillis}.js"
    val xml = <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        <meta name="description" content="Seqexec"/>
        <meta name="author" content="Gemini Software Group"/>
        <link rel="icon" href={s"images/launcher.${builtAtMillis}.ico"}/>

        <!-- Add to homescreen for Safari on iOS -->
        <meta name="apple-mobile-web-app-capable" content="yes"/>
        <meta name="apple-mobile-web-app-status-bar-style" content="black"/>
        <meta name="apple-mobile-web-app-title" content="Seqexec"/>
        <link rel="apple-touch-icon-precomposed" href={s"images/launcher.${builtAtMillis}.png"}/>

        <title>Seqexec</title>

        <link rel="stylesheet" href={s"css/semantic.${builtAtMillis}.css"}/>
        <style>{style.stripMargin}</style>
      </head>

      <body>

        <div id="content">
        </div>

        <script src={deps}></script>
        <script src={seqexecScript}></script>
        <script type="text/javascript">
          edu.gemini.seqexec.web.client.SeqexecApp().main();
        </script>
      </body>
    </html>
    s"<!DOCTYPE html>$xml"
  }

}
