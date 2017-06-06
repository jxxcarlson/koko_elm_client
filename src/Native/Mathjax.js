
// var MathJax = require('MathJax');

var _user$project$Native_Mathjax = function() {

// VIRTUAL-DOM WIDGETS

function toHtml(options, factList, rawTex)
{
	var model = {
		options: options,
		tex: rawTex
	};
	return _elm_lang$virtual_dom$Native_VirtualDom.custom(factList, model, implementation);
}

// WIDGET IMPLEMENTATION

var implementation = {
	render: render,
	diff: diff
};

// XXX: FUNCTION RENDER
function render(model)
{
	// var html = texed(model.tex, formatOptions(model.options));
	// var div = document.createElement('div');
	// div.innerHTML = html;
	// return div;

  var div = MathJax.HTML.Element(
  "div",
  {id: "MathDiv", style:{border:"1px solid", padding:"5px"}},
  [model.tex]
);

return div
}

function diff(a, b)
{

	if (a.model.tex === b.model.tex && a.model.options === b.model.options)
	{
		return null;
	}

	return {
		applyPatch: applyPatch,
		data: texed(b.model.tex, formatOptions(b.model.options))
	};
}

function applyPatch(domNode, data)
{
	domNode.innerHTML = data;
	return domNode;
}


// ACTUAL TEX PARSER

// XXX: FUNCTION TEXED
var texed = function() {
	// catch the `marked` object regardless of the outer environment.
	// (ex. a CommonJS module compatible environment.)
	// note that this depends on marked's implementation of environment detection.
	var module = {};
	var exports = module.exports = {};

	/// xxx//

	return module.exports;
}();


// FORMAT OPTIONS FOR MARKED IMPLEMENTATION

function formatOptions(options)
{

}


// EXPORTS

return {
	toHtml: F3(toHtml)
};

}();
