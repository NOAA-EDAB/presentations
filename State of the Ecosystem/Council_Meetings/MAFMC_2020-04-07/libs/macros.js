remark.macros.img = function (altText, percentage) {
  var url = this;
  return '<img alt="' + altText + '" src="' + url + '" style="width: ' + percentage + '" />';
};