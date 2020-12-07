import "./theme/semantic.less";
import "./less/style.less";
import "./less/semantic-ui-alerts.less";

var App = require("sjs/seqexec_web_client-fastopt.js");

if (module.hot) {
  module.hot.dispose(() => {
    App.SeqexecApp.stop();
  });
  module.hot.accept();
  App.SeqexecApp.start();
}
