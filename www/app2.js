

var pagecount = 1;

var pokeidcount = 1;




function leftbut(id){
  pagecount = Math.max(pagecount - 1,1);
  Shiny.setInputValue('page_count', pagecount);
}

function rightbut(id){
  pagecount = Math.min(pagecount + 1,5);
  Shiny.setInputValue('page_count', pagecount);
}

function upbut(id){
  pokeidcount = pokeidcount + 1;
  Shiny.setInputValue('id_count', pokeidcount);
  Shiny.setInputValue('updown_but', 'up');
}

function downbut(id){
  pokeidcount = pokeidcount + 1;
  Shiny.setInputValue('id_count', pokeidcount);
  Shiny.setInputValue('updown_but', 'down');
}





Shiny.addCustomMessageHandler('background-type', function(color) {

  
  document.getElementsByClassName('typecard')[0].style.backgroundColor = color;
  document.getElementById('sel1-label').style.color = color;
  document.getElementById('sel1-label').style.color = color;
  document.getElementById('sel1-label').style.fontSize = "200%";  

});


Shiny.addCustomMessageHandler('background-type2', function(color) {

  document.getElementsByClassName('typecard')[1].style.backgroundColor = color;
  
});
      
Shiny.addCustomMessageHandler('hide-type2', function(color) {

  document.getElementById('type2card').style.display = "none";

});

// Location





Shiny.addCustomMessageHandler('update_location', function(x) {


        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.setInputValue('user_lat', coords.latitude);
            Shiny.setInputValue('user_lon', coords.longitude);
          }, 1100)
        }


});


/// rival


Shiny.addCustomMessageHandler('background-type-rival', function(color) {

  
  document.getElementById('typecard2').style.backgroundColor = color;
  document.getElementById('sel2-label').style.color = color;
  document.getElementById('sel2-label').style.color = color;
  document.getElementById('sel2-label').style.fontSize = "200%";  

});


Shiny.addCustomMessageHandler('background-type2-rival', function(color) {

  document.getElementById('type2card2').style.backgroundColor = color;
  
});
      
Shiny.addCustomMessageHandler('hide-type2-rival', function(color) {

  document.getElementById('type2card2').style.display = "none";

});
    
    