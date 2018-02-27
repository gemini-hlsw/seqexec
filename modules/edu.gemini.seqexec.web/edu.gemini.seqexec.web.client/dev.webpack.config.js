const Path = require("path");
const Webpack = require("webpack");
const Merge = require("webpack-merge");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const Autoprefixer = require("autoprefixer");

const Common = require("./common.webpack.config.js");
const isDevServer = process.argv.some(s => s.match(/webpack-dev-server\.js$/));

const Web = Merge(Common.Web, {
  mode: "development",
  output: {
    path: __dirname,
    publicPath: "/"
  },
  entry: {
    app: [Path.resolve(Common.resourcesDir, "./dev.js")]
  },
  module: {
    noParse: function(content) {
      return content.endsWith("-fastopt");
    },
    rules: [
      {
        test: /\.less$/,
        use: [
          {
            loader: "style-loader" // creates style nodes from JS strings
          },
          // translates CSS into CommonJS
          { loader: "css-loader", options: { importLoaders: 1 } },
          {
            loader: "postcss-loader", // post css plugins
            options: {
              ident: "postcss",
              plugins: loader => [
                Autoprefixer({
                  browsers: ["last 3 versions", "> 1%"]
                })
              ]
            }
          },
          {
            loader: "less-loader" // compiles Less to CSS
          }
        ]
      }
    ]
  },
  devServer: {
    hot: true,
    contentBase: [__dirname, Common.rootDir],
    historyApiFallback: true,
    // Proxy targets to the api server
    proxy: {
      "/api/seqexec/events": {
        target: "http://localhost:9090",
        changeOrigin: true,
        ws: true
      },
      "/api/***": {
        target: "http://localhost:9090",
        logLevel: "debug",
        bypass: function(req, res, proxyOptions) {
          // regex matching everything but js files
          var backendUrls = /(^(.(?!\.js$))+$)/;

          if (!backendUrls.test(req.url)) {
            console.log("other: " + req.url);
            return req.url;
          } else {
            console.log("proxied: " + req.url);
          }
        }
      },
      watchOptions: {
        // We need this to get around the long scala.js compilation cycles
        aggregateTimeout: 35000
      }
      /*"/***": {
        target: "http://localhost:9090",
        logLevel: "debug",
        bypass: function(req, res, proxyOptions) {
          // regex matching everything but js files
          var backendUrls = /(^(.(?!\.js$))+$)/;

          if (!backendUrls.test(req.url)) {
            console.log("other: " + req.url);
            return req.url;
          } else {
            console.log("proxied: " + req.url);
          }
        }
      }*/
    }
  },
  plugins: [
    new Webpack.HotModuleReplacementPlugin(),
    new Webpack.NamedModulesPlugin()
    // new HtmlWebpackPlugin({
    //   filename: "index.html",
    //   chunks: ["app"],
    //   template: Path.resolve(Common.resourcesDir, "./index.html"),
    //   favicon: Path.resolve(Common.resourcesDir, "./images/favicon.ico")
    // })
  ]
});

if (isDevServer) {
  Web.entry.app.push("webpack-dev-server-status-bar");
}

console.log(Web);
module.exports = Web;
