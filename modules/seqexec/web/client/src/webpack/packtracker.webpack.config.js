const Merge = require("webpack-merge");
const Web = require("./prod.webpack.config");
const PacktrackerPlugin = require("@packtracker/webpack-plugin");

const PackTracker = Merge(Web, {
  plugins: [
    new PacktrackerPlugin({
      project_token: "eb5ee661-6208-4044-a706-85aebe3f774d",
      upload: true,
      fail_build: true,
      branch: process.env.GITHUB_REF.split("/")[2],
      exclude_assets: [/seqexec_web_client-opt.*/]
    })
  ]
});

module.exports = PackTracker;
