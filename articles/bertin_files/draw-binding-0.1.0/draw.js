HTMLWidgets.widget({

  name: 'draw',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

      // let container = el.appendChild("div");
        //container.style.width = "100%"
        el.appendChild(bertin.draw(JSON.parse(x.message)));
        el.style.width = "100%"


      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
