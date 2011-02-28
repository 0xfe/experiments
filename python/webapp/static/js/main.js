function setup_validation() {
    var register_form = $("#register_form");
    register_form.validate();
    $("#repeat_password").rules("add", { equalTo: "#password" });
}

require(["jquery.validate.js"], function(_) {
    $(function() { setup_validation() });
});
