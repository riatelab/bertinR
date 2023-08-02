HTMLWidgets.widget({
  name: "draw",

  type: "output",

  //factory: function(el, width, height) {
  factory: function(el, width, height) {

    var map;

    return {
      renderValue: function(x) {
        el.innerText = "";
        if (!x.parameters.params.hasOwnProperty("width")) {
          x.parameters.params.width = width;
        }
        map = bertin.draw(x.parameters);
        el.appendChild(map);
        //el.style.width = "100%";
      },
      getMap: function() {
        return map;
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});


function getBertin(id) {
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);
  var map;
  if (typeof htmlWidgetsObj != "undefined") {
    map = htmlWidgetsObj.getMap();
  }
  return map;
}

if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler("bertin-update", function(message) {
    var map = getBertin(message.id);
    if (typeof map != "undefined") {
      map.update(message.data);
    }
  });
}
