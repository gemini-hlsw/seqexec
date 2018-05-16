const path = require("path");
const Merge = require("webpack-merge");
const PostCSSPlugin = require("postcss-loader");
const Autoprefixer = require("autoprefixer");

const generatedConfig = require("./scalajs.webpack.config");
const parts = require("./webpack.parts");

const Web = Merge(generatedConfig, {
  devtool: "none",
  resolve: {
    alias: {
      resources: parts.resourcesDir,
      // Required to find the Semantic-UI-less module
      node_modules: path.resolve(__dirname, "node_modules"),
      // Used to find the produced scala.js file
      root: __dirname,
      // Required for the custom Semantic UI theme
      "../../theme.config$": path.join(parts.resourcesDir, "theme/theme.config")
    },
    modules: [path.resolve(__dirname, "node_modules"), parts.resourcesDir]
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
  }
});

module.exports = {
  Web: Web
};
