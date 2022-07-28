$(document).on("shiny:busy", function() {
  var inputs = document.getElementsByTagName("button", "select");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = true;
}
});

$(document).on("shiny:idle", function() {
  var inputs = document.getElementsByTagName("button", "select");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = false;
}
});
