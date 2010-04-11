// ==UserScript==
// @name Renamer
// @namespace http://muthanna.com/greasemonkey
// @include http://diveintogreasemonkey.org/*
// @include http://www.diveintogreasemonkey.org/*
// ==/UserScript==

GM_log('Running on ' + window.location.href);

var myDiv = document.getElementById('intro');

var link = myDiv.childNodes[1].childNodes[1].childNodes[1].childNodes[1];

link.innerHTML = "You've Been Owned";
