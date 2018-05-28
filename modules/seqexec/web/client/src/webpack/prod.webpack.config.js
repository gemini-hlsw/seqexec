const path = require("path");
const Merge = require("webpack-merge");
const Webpack = require("webpack");
const parts = require("./webpack.parts");
const ScalaJSConfig = require("./scalajs.webpack.config");

const HtmlWebpackPlugin = require("html-webpack-plugin");

const Web = Merge(
  ScalaJSConfig,
  parts.resolve,
  parts.resolveSemanticUI,
  parts.resourceModules,
  parts.extractCSS({
    devMode: false,
    use: ["css-loader", "less-loader"] // parts.autoprefix()] // autoprefix needs a fix on scalajs-bundler
  }),
  parts.minifyJavaScript(),
  parts.minifyCSS({}),
  parts.extraAssets,
  parts.fontAssets,
  {
    mode: "production",
    entry: {
      seqexec: [path.resolve(parts.resourcesDir, "./prod.js")]
    },
    output: {
      filename: "[name].[chunkhash].js",
      publicPath: "/" // Required to make the url navigation work
    },
    plugins: [
      // Useful to further minify react and make it faster in production
      new Webpack.DefinePlugin({
        "process.env": {
          NODE_ENV: JSON.stringify("production")
        }
      }),
      new HtmlWebpackPlugin({
        title: "Seqexec",
        filename: "index.html",
        chunks: ["seqexec"]
      })
    ]
  }
);

module.exports = Web;
