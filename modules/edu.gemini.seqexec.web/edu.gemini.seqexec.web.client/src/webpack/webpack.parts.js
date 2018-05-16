/**
 * Sheareable function to configure webpack
 * this should be generic enough to be usable across scala.js projects
 */
const path = require("path");

const rootDir = path.resolve(__dirname, "../../../..");
const resourcesDir = path.resolve(rootDir, "src/main/resources");

const Webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const cssnano = require("cssnano");

module.exports.rootDir = rootDir;
module.exports.resourcesDir = resourcesDir;

module.exports.devServer = ({ host, port } = {}) => ({
  devServer: {
    stats: "errors-only",
    host, // Defaults to `localhost`
    port, // Defaults to 8080
    overlay: true,
    historyApiFallback: true,
    contentBase: [__dirname, rootDir],
    hot: true,
    historyApiFallback: true
  },
  module: {
    noParse: function(content) {
      return content.endsWith("-fastopt");
    }
  },
  plugins: [new Webpack.HotModuleReplacementPlugin()]
});

module.exports.extractCSS = ({ devMode, include, exclude, use = [] }) => {
  // Output extracted CSS to a file
  const plugin = new MiniCssExtractPlugin({
    filename: devMode ? "[name].css" : "[name].[contenthash].css",
    chunkFilename: devMode ? "[id].css" : "[id].[contenthash].css"
  });

  return {
    module: {
      rules: [
        {
          test: /\.less$|\.css$/,
          include,
          exclude,
          use: [devMode ? "style-loader" : MiniCssExtractPlugin.loader].concat(
            use
          )
        }
      ]
    },
    plugins: [plugin]
  };
};

module.exports.resolve = () => ({
  resolve: {
    alias: {
      // resources: resourcesDir,
      // node_modules: path.resolve(__dirname, "node_modules"),
      sjs: __dirname
    }
    //   modules: [path.resolve(__dirname, "node_modules"), parts.resourcesDir]
  }
});

exports.autoprefix = () => ({
  loader: "postcss-loader",
  options: {
    plugins: () => [require("autoprefixer")()]
  }
});

exports.minifyCSS = ({ options }) => ({
  plugins: [
    new OptimizeCSSAssetsPlugin({
      cssProcessor: cssnano,
      cssProcessorOptions: options,
      canPrint: false
    })
  ]
});
