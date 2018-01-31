const Path = require("path");
const Merge = require("webpack-merge");
const ExtractTextPlugin = require("extract-text-webpack-plugin");

const generatedConfig = require("./scalajs.webpack.config");

const rootDir = Path.resolve(__dirname, "../../../..");
const resourcesDir = Path.resolve(rootDir, "src/main/resources");

const Web = Merge(generatedConfig, {
  devtool: "none",
  resolve: {
    alias: {
      // resources: resourcesDir,
      // Required to find the Semantic-UI-less module
      node_modules: Path.resolve(__dirname, "node_modules"),
      // Used to find the produced scala.js file
      root: __dirname,
      // Required for the custom Semantic UI theme
      "../../theme.config$": Path.join(resourcesDir, "theme/theme.config")
    },
    modules: [Path.resolve(__dirname, "node_modules"), resourcesDir]
  },
  module: {
    rules: [
      {
        test: /\.jpe?g$|\.gif$|\.png$|\.ttf$|\.eot$|\.svg$|.mp3$/,
        loader: "file-loader",
        options: {
          name: "[name].[hash].[ext]"
        }
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: "url-loader?limit=10000&mimetype=application/fontwoff"
      }
    ]
  },
  plugins: [
    new ExtractTextPlugin({
      filename: "[name].[contenthash].css"
    })
  ]
});

module.exports = {
  rootDir: rootDir,
  resourcesDir: resourcesDir,
  Web: Web
};
