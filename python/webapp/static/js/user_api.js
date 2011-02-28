User = function(url) {
  this.init(url)
}

User.prototype.init = function(url) {
  this.url = url;
}

User.prototype.login = function(email, handler) {
  Vex.Ajax(this.url, { 'action': 'ajax_login', 'email': email }, handler);
}

User.prototype.register = function(email, name, password, handler) {
  Vex.Ajax(this.url, 
           { 'action': "ajax_register",
             'email':email,
             'name': name,
             'password': password
           }, handler);
}
