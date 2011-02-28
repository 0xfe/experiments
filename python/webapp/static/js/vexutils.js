// Vex Tools for JavaScript
// Mohit Muthanna <mohit@muthanna.com>
//
// Tools, utility functions, and other shared JS cruft.
//
// Requires: jQuery

/*
   function Vex.IsIE()

   Returns true if we're running in Internet Explorer.
*/
Vex.IsIE = function() {
  return /msie/i.test(
      navigator.userAgent) && !/opera/i.test(navigator.userAgent);
}

/*
  function Vex.Merge(dest, source)

  Merge the associative arrays (or objects) in dest and source. Modifies "dest"
  and returns its value.
*/
Vex.Merge = function(destination, source) {
    for (var property in source)
        destination[property] = source[property];
    return destination;
};


Vex.error_sel = "#error-message";

Vex.SetupAjaxLoader = function(id) {
  $(id).hide();

  $(id).ajaxStart(function() {
    $(id).show();
  });

  $(id).ajaxStop(function() {
    $(id).hide();
  });
}

Vex.GetCookie = function(name) {
    var r = document.cookie.match("\\b" + name + "=([^;]*)\\b");
    return r ? r[1] : undefined;
}

/*
   function Vex.Ajax(url, data, handler);

   Call AJAX method in "url" using POST and data provided in "data". Upon
   success or error, calls:

      handler(success, data, textstatus, xmlhttprequest, error)

   where:

      success: bool (false if AJAX or API error)
      data: result
      textstatus: status code
        (One of: "timeout", "error", "notmodified", "parseerror", "apierror")
      xmlhttprequest: the request object
      error: an optional exception

   The AJAX method should return one of the following:

      1. Upon success:   { success: true, data: {...} }
      2. Upon error:     { success: false, message: "string" }

   Vex.error_sel (if set) is populated with the error message upon error.
*/
Vex.Ajax = function(url, data, handler) {
  data._xsrf = Vex.GetCookie("_xsrf");
  Vex.LogDebug("Ajax request:");
  Vex.LogDebug(data);

  $.ajax({
    type: "POST",
    'url': url,
    'data': data,
    'dataType': 'json',
    success: function(d, t, x) {
      if (!d) {
        Vex.L("Server Error: " + t + " in " + x);
        $(Vex.error_sel).text(
          "Unexpected server error. Please try again later.");
        $(Vex.error_sel).fadeIn(500).delay(3000).fadeOut(1000);
        handler(false, null, "servererror", x, null);
        return;
      }

      if (!d.success) {
        var message = ""
        for (var i = 0; i < d.errors.length; ++i) {
          message += d.errors[i] + "\n";
        }

        Vex.L("API Error: " + message);
        Vex.LogDebug("Ajax response:");
        Vex.LogDebug(d);
        $(Vex.error_sel).text("(API Error) " + message);
        $(Vex.error_sel).fadeIn(500).delay(3000).fadeOut(1000);
        handler(false, d, "apierror", x, null);
      } else {
        Vex.LogDebug("Ajax response:");
        Vex.LogDebug(d);
        handler(true, d, t, x, null);
      }
    },
    error: function(x, t, e) {
      Vex.L("Server Error: " + t);
      $(Vex.error_sel).text("(Server Error) " + t);
      $(Vex.error_sel).fadeIn(500).delay(3000).fadeOut(1000);
      handler(false, null, t, x, e);
    }
  });
}

/*
  Vex.InstallTracker - Install an asynchronous Google Analytics Tracker
  with the property ID supplied in "property_id".

  See http://google.com/analytics for details.
*/
Vex.InstallTracker = function(property_id) {
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', property_id]);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script');
    ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ?
      'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(ga, s);
  })();
}

function html_escape(message) {
  return $('<div/>').text(message).html();
}

