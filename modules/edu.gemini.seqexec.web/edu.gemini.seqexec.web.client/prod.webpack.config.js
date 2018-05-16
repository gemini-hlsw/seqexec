const Path = require("path");
const UglifyJsPlugin = require("uglifyjs-webpack-plugin");
const Merge = require("webpack-merge");
const Webpack = require("webpack");

const Common = require("./common.webpack.config.js");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const Web = Merge(Common.Web, {
  mode: "production",
  entry: {
    app: [Path.resolve(Common.resourcesDir, "./prod.js")]
  },
  output: {
    filename: "[name].[chunkhash].js"
  },
  optimization: {
    minimize: false
  },
  plugins: [
    // Useful to further minify react and make it faster in production
    new Webpack.DefinePlugin({
      "process.env": {
        NODE_ENV: JSON.stringify("production")
      }
    }),
    // Reduce the size of the library
    // new UglifyJsPlugin({
    //   sourceMap: module.exports.devtool === "source-map"
    // }),
    // Manually copy the files. could be improved by scalajs-bundler
    new CopyWebpackPlugin([
      { from: "*.mp3", to: Common.resourcesManagedDir },
      { from: "*.css", to: Common.resourcesManagedDir },
      { from: "*.html", to: Common.resourcesManagedDir },
      { from: "*.js", to: Common.resourcesManagedDir }
    ]),
    new HtmlWebpackPlugin({
      filename: "index.html",
      chunks: ["app"],
      template: Path.resolve(Common.resourcesDir, "./index.html")
    })
  ]
});

module.exports = Web;
