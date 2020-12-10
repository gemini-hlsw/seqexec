import { SeqexecApp }  from  "@sjs/main.js";
// import "/semantic.less";
// import "./less/style.less";
// import "./less/semantic-ui-alerts.less";
import '/public/semantic.css';
import '/public/style.css';

if (import.meta.hot) {
  import.meta.hot.dispose(() => {
    SeqexecApp.stop();
  });
  import.meta.hot.accept(({module}) => {
    console.log("CH")
    console.log(module)
  });
  SeqexecApp.start();
}
