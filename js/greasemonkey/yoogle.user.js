// ==UserScript==
// @name Renamer
// @namespace http://muthanna.com/greasemonkey
// @include http://google.com/*
// @include http://www.google.com/*
// ==/UserScript==

var center = document.evaluate('//center',
                               document,
                               null,
                               XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,
                               null);

image = center.snapshotItem(0).childNodes[1];

image.width = 232
image.height = 44
image.src = "http://us.i1.yimg.com/us.yimg.com/i/ww/beta/y3.gif"
