const Merge = require("webpack-merge");
const parts = require("./webpack.parts");

const Test = Merge(parts.extraAssets, parts.resourceModules);

module.exports = Test;
module.exports.node = {
  fs: "empty"
};
