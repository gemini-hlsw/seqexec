const Merge = require("webpack-merge");
const Web = require("./prod.webpack.config");
const PacktrackerPlugin = require("@packtracker/webpack-plugin");

console.log(process.env);

const PackTracker = Merge(Web, {
  plugins: [
    new PacktrackerPlugin({
      project_token: process.env.PT_PROJECT_TOKEN,
      upload: true,
      fail_build: true,
      branch: process.env.GITHUB_REF.split("/")[2],
      excludeAssets: ["seqexec_web_client-opt.js"]
    })
  ]
});

module.exports = PackTracker;
