// Requires jQuery

function Notifier() {}

// Request permission for this page to send notifications. If allowed,
// calls function "cb" with true.
Notifier.prototype.RequestPermission = function(cb) {
  window.webkitNotifications.requestPermission(function() {
      if (window.webkitNotifications.checkPermission() == 0) {
        return true;
      }
  });
}

// Popup a notification with icon, title, and body. Returns false if
// permission was not granted.
Notifier.prototype.Notify = function(icon, title, body) {
  if (window.webkitNotifications.checkPermission() == 0) {
    var popup = window.webkitNotifications.createNotification(
        icon, title, body);
    popup.show();

    return true;
  }

  return false;
}
