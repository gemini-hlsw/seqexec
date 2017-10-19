var UglifyJsPlugin = require('uglifyjs-webpack-plugin');
var webpack = require('webpack');

module.exports = require('./scalajs.webpack.config');

module.exports.plugins = (module.exports.plugins || []).concat([
  // Useful to further minify react and make it faster in production
  new webpack.DefinePlugin({
    'process.env': {
      NODE_ENV: JSON.stringify('production')
    }
  }),
  // Reduce the size of the library
  new UglifyJsPlugin({
    sourceMap: module.exports.devtool === 'source-map'
  })
]);
