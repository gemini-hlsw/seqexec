const path = require("path");
const Webpack = require("webpack");
const Merge = require("webpack-merge");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const parts = require("./webpack.parts");

const Common = require("./common.webpack.config.js");
const isDevServer = process.argv.some(s => s.match(/webpack-dev-server\.js$/));

const Web = Merge(
  Common.Web,
  parts.extractCSS({ devMode: true, use: ["css-loader", "less-loader"] }),
  {
    mode: "development",
    entry: {
      app: [path.resolve(parts.resourcesDir, "./dev.js")]
    },
    module: {
      noParse: function(content) {
        return content.endsWith("-fastopt");
      }
    },
    devServer: {
      hot: true,
      contentBase: [__dirname, parts.rootDir],
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
      }
    },
    plugins: [
      new Webpack.HotModuleReplacementPlugin(),
      new HtmlWebpackPlugin({
        filename: "index.html",
        chunks: ["app"],
        template: path.resolve(parts.resourcesDir, "./index.html"),
        favicon: path.resolve(parts.resourcesDir, "./images/favicon.ico")
      })
    ]
  }
);

if (isDevServer) {
  Web.entry.app.push("webpack-dev-server-status-bar");
}

module.exports = Web;
