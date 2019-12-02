const path = require("path");
const Merge = require("webpack-merge");
const Webpack = require("webpack");
const parts = require("./webpack.parts");
const ScalaJSConfig = require("./scalajs.webpack.config");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");

const HtmlWebpackPlugin = require("html-webpack-plugin");

const ci = process.env.CI; // When on CI don't add hashes

const Web = Merge(
  ScalaJSConfig,
  parts.resolve,
  parts.resolveSemanticUI,
  parts.resourceModules,
  parts.extractCSS({
    devMode: false,
    use: ["css-loader", parts.autoprefix(), "less-loader"], // Order is very important: css, post-css, less
    ci: ci
  }),
  parts.minifyJavaScript(),
  parts.minifyCSS({
    options: {
      safe: true,
      mergeLonghand: false, // Required to avoid merges of border properties that are unsafe
      discardComments: { removeAll: true },
      autoprefixer: { disable: true } // Otherwise this conflicts with post-css autoprefixer
    }
  }),
  parts.extraAssets,
  parts.fontAssets,
  {
    mode: "production",
    entry: {
      seqexec: [path.resolve(parts.resourcesDir, "./prod.js")]
    },
    output: {
      filename: ci ? "[name].js" : "[name].[chunkhash].js",
      publicPath: "/" // Required to make url navigation work
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
      }),
      new FaviconsWebpackPlugin({
        logo: path.resolve(parts.resourcesDir, "images/launcher.png"),
        persistentCache: false
      })
    ]
  }
);

module.exports = Web;
