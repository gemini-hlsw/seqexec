import "./theme/semantic.less";
import "./less/style.less";
import React from "react";

// Enable why did you update plugin
if (process.env.NODE_ENV !== "production") {
  const { whyDidYouUpdate } = require("why-did-you-update");
  whyDidYouUpdate(React, {
    exclude: ["Draggable", "DraggableCore", "AutoSizer", "DiodeWrapper", "Button", "Icon", "Progress", "Login", "Ref", "RefFindNode"]
  });
}

var App = require("sjs/seqexec_web_client-fastopt.js");

if (module.hot) {
  module.hot.dispose(() => {
    App.SeqexecApp.stop();
  });
  module.hot.accept();
  App.SeqexecApp.start();
}
