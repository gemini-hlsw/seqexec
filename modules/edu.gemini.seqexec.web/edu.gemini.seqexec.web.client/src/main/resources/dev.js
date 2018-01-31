import "node_modules/semantic-ui-less/semantic.less";
import "./less/style.less";

var App = require("root/edu_gemini_seqexec_web_client-fastopt.js");

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
  module.hot.accept();
  App.seqexec.SeqexecApp.start("GS");
}
