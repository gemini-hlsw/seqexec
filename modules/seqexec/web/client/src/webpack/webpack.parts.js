/**
 * Sheareable function to configure webpack
 * this should be generic enough to be usable across scala.js projects
 */
const path = require("path");

const Webpack = require("webpack");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const UglifyWebpackPlugin = require("uglifyjs-webpack-plugin");
const cssnano = require("cssnano");

// Dir at the top of the module. e.g. edu_gemini_seqexec_web_client
const rootDir = path.resolve(__dirname, "../../../..");
module.exports.rootDir = rootDir;

// Resources dir on sbt
const resourcesDir = path.resolve(rootDir, "src/main/resources");
module.exports.resourcesDir = resourcesDir;

// Set of browser to support on the css. Taken from Semantic-UI-React
module.exports.browsers = {
  browsers: [
    ">1%",
    "last 4 versions",
    "Firefox ESR",
    "not ie < 9" // React doesn't support IE8 anyway
  ],
  flexbox: "no-2009"
};

// Setup webpack-dev-server. Use only in development
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

module.exports.lessLoader = (options = {}) => {
  return {
    loader: "less-loader",
    options: { sourceMap: true }
  };
};

// Extract css to a file, use only in production
module.exports.extractCSS = ({
  devMode,
  include,
  exclude,
  use = [],
  ci = false
}) => {
  const filename = ci ? "[name].css" : "[name].[contenthash].css";
  const chunkFilename = ci ? "[id].css" : "[id].[contenthash].css";
  // Output extracted CSS to a file
  const plugin = new MiniCssExtractPlugin({
    filename: devMode ? "[name].css" : filename,
    chunkFilename: devMode ? "[id].css" : chunkFilename
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

// Enable autoprefixing with postcss
exports.autoprefix = () => {
  return {
    loader: "postcss-loader",
    options: {
      autoprefixer: module.exports.browsers,
      plugins: () => [require("autoprefixer")()]
    }
  };
};

// This is needed for scala.js projects
module.exports.resourceModules = {
  resolve: {
    modules: [path.resolve(__dirname, "node_modules"), resourcesDir]
  }
};

// Let webpack find assets on sbt paths
module.exports.resolve = {
  resolve: {
    alias: {
      // Find files on resources
      resources: resourcesDir,
      // Used to find the produced scala.js file
      sjs: __dirname
    }
  }
};

// Support for loading semantic ui themes and less configuration
module.exports.resolveSemanticUI = {
  resolve: {
    alias: {
      // Required for the custom Semantic UI theme
      "../../theme.config$": path.join(resourcesDir, "theme/theme.config")
    }
  }
};

// Support css minifications
exports.minifyCSS = ({ options }) => ({
  plugins: [
    new OptimizeCSSAssetsPlugin({
      cssProcessor: cssnano,
      cssProcessorOptions: options,
      canPrint: false
    })
  ]
});

// Support js minification
exports.minifyJavaScript = () => ({
  optimization: {
    minimize: true,
    minimizer: [
      new UglifyWebpackPlugin({
        parallel: true,
        uglifyOptions: { mangle: true },
        sourceMap: false
      })
    ]
  }
});

// Loader for fonts
exports.fontAssets = {
  module: {
    rules: [
      {
        // Match woff2 in addition to patterns like .woff?v=1.1.1.
        test: /\.(woff|woff2)(\?v=\d+\.\d+\.\d+)?$/,
        use: {
          loader: "url-loader",
          options: {
            // Limit at 50k. Above that it emits separate files
            limit: 50000,

            // url-loader sets mimetype if it's passed.
            // Without this it derives it from the file extension
            mimetype: "application/font-woff",

            // Output below fonts directory
            name: "[name].[hash].[ext]"
          }
        }
      }
    ]
  }
};

// Loads assets as files including images, audio and old style fonts
exports.extraAssets = {
  module: {
    rules: [
      {
        test: /\.jpe?g$|\.gif$|\.png$|\.ttf$|\.eot$|\.svg$|.mp3$/,
        loader: "file-loader",
        options: {
          name: "[name].[hash].[ext]"
        }
      }
    ]
  }
};
