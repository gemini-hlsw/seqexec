import "./theme/semantic.less";
import "./less/style.less";
import React from "react";

// Enable why did you update plugin
if (process.env.NODE_ENV !== "production") {
  const { whyDidYouUpdate } = require("why-did-you-update");
  whyDidYouUpdate(React);
}

var App = require("sjs/seqexec_web_client-fastopt.js");

// Required to enable semantic-ui jQuery extensions
var $ = require("jquery");
$.fn.dropdown = require("semantic-ui-dropdown");
$.fn.visibility = require("semantic-ui-visibility");
$.fn.tab = require("semantic-ui-tab");
$.fn.progress = require("semantic-ui-progress");
$.fn.dimmer = require("semantic-ui-dimmer");
$.fn.transition = require("semantic-ui-transition");
$.fn.modal = require("semantic-ui-modal");
$.fn.popup = require("semantic-ui-popup");

if (module.hot) {
  module.hot.dispose(data => {
    App.seqexec.SeqexecApp.stop();
  });
  module.hot.accept();
  App.seqexec.SeqexecApp.start();
}
