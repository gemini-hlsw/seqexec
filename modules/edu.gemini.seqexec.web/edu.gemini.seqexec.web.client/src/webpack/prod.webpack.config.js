const path = require("path");
const Merge = require("webpack-merge");
const Webpack = require("webpack");
const parts = require("./webpack.parts");

const Common = require("./common.webpack.config.js");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const resourcesManagedDir = path.resolve(
  parts.rootDir,
  "target/scala-2.12/resource_managed/main"
);

const Web = Merge(
  Common.Web,
  parts.extractCSS({
    devMode: false,
    use: ["css-loader", "less-loader"] // parts.autoprefix()] //, parts.minifyCSS()]
  }),
  {
    mode: "production",
    entry: {
      app: [path.resolve(parts.resourcesDir, "./prod.js")]
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
      // new CopyWebpackPlugin([
      //   { from: "*.mp3", to: resourcesManagedDir },
      //   { from: "*.css", to: resourcesManagedDir },
      //   { from: "*.html", to: resourcesManagedDir },
      //   { from: "*.js", to: resourcesManagedDir }
      // ]),
      new HtmlWebpackPlugin({
        title: "Seqexec",
        filename: "index.html",
        chunks: ["app"]
        // template: path.resolve(parts.resourcesDir, "./index.html")
      })
    ]
  }
);

module.exports = Web;
