import "./theme/semantic.less";
import "./less/style.less";
var App = require("sjs/seqexec_web_client-opt.js");

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

App.SeqexecApp.start();
