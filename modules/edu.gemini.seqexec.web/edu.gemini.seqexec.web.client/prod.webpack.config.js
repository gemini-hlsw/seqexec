const Path = require("path");
const UglifyJsPlugin = require("uglifyjs-webpack-plugin");
const Merge = require("webpack-merge");
const Webpack = require("webpack");

const Common = require("./common.webpack.config.js");
const CopyWebpackPlugin = require("copy-webpack-plugin");

const Web = Merge(Common.Web, {
  mode: "production",
  entry: {
    app: [Path.resolve(Common.resourcesDir, "./prod.js")]
  },
  plugins: [
    // Useful to further minify react and make it faster in production
    new Webpack.DefinePlugin({
      "process.env": {
        NODE_ENV: JSON.stringify("production")
      }
    }),
    // Reduce the size of the library
    new UglifyJsPlugin({
      sourceMap: module.exports.devtool === "source-map"
    }),
    new CopyWebpackPlugin(
      [
        { from: "*.mp3", to: Common.resourcesManagedDir },
        { from: "*.css", to: Common.resourcesManagedDir }
      ],
      {
        debug: "info"
      }
    )
  ]
});

console.log(Web);
console.log(Common.resourcesManagedDir);
module.exports = Web;
