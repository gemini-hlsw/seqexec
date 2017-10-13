// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server

package object http4s {
  // scalastyle:off
  def index(site: String, devMode: Boolean, builtAtMillis: Long): String = {
    val style = """
                  |   @media screen and (-webkit-min-device-pixel-ratio:0) {
                  |        select,
                  |        textarea,
                  |        input {
                  |          font-size: 16px !important;
                  |        }
                  |      }
                  |"""
    val deps = if (devMode) "edu_gemini_seqexec_web_client-fastopt-library.js" else s"edu_gemini_seqexec_web_client-opt-library.$builtAtMillis.js"
    val loaderScript = if (devMode) s"edu_gemini_seqexec_web_client-fastopt-loader.js" else s"edu_gemini_seqexec_web_client-opt-loader.$builtAtMillis.js"
    val seqexecScript = if (devMode) s"edu_gemini_seqexec_web_client-fastopt.js" else s"edu_gemini_seqexec_web_client-opt.$builtAtMillis.js"
    val xml =
      <html lang="en">
        <head>
          <meta charset="utf-8"/>
          <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
          <meta name="viewport" content="width=device-width, initial-scale=1"/>
          <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
          <meta name="description" content={s"Seqexec - $site"}/>
          <meta name="author" content="Gemini Software Group"/>
          <link rel="icon" href={s"/images/launcher.$builtAtMillis.ico"}/>

          <!-- Add to homescreen for Safari on iOS -->
          <meta name="apple-mobile-web-app-capable" content="yes"/>
          <meta name="apple-mobile-web-app-status-bar-style" content="black"/>
          <meta name="apple-mobile-web-app-title" content="Seqexec"/>
          <link rel="apple-touch-icon-precomposed" href={s"/images/launcher.$builtAtMillis.png"}/>

          <title>{s"Seqexec - $site"}</title>

          <link rel="stylesheet" href={s"/css/semantic.$builtAtMillis.css"}/>
          <style>{style.stripMargin}</style>
        </head>

        <body>

          <div id="content">
          </div>

          <script src={s"/$deps"}></script>
          <script src={s"/$loaderScript"}></script>
          <script>
            {"""
              /* Trick to get semantic ui to talk to jquery loaded via modules */
              var $ = require('jquery');
              $.fn.dropdown = require('semantic-ui-dropdown');
              $.fn.visibility = require('semantic-ui-visibility');
              $.fn.tab = require('semantic-ui-tab');
              $.fn.progress = require('semantic-ui-progress');
              $.fn.dimmer = require('semantic-ui-dimmer');
              $.fn.transition = require('semantic-ui-transition');
              $.fn.modal = require('semantic-ui-modal');
              """} </script>
          <script src={s"/$seqexecScript"}></script>
          <script>
            {s"""SeqexecApp.start('$site');"""}
          </script>
        </body>
      </html>
    s"<!DOCTYPE html>$xml"
  }
  // scalastyle:on

}
