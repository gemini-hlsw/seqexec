const path = require("path");
const Webpack = require("webpack");
const Merge = require("webpack-merge");
const parts = require("./webpack.parts");

const Test = Merge(parts.extraAssets, parts.resourceModules);

module.exports = Test;
