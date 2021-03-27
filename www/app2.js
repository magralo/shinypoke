

function upred(id) {
  Shiny.setInputValue('prueba', 'red');
}

function upyellow(id) {
  Shiny.setInputValue('prueba', 'yellow');
}


function upgreen(id) {
  Shiny.setInputValue('prueba', 'green');
}

function upblack(id) {
  Shiny.setInputValue('prueba', 'black');
}




Shiny.addCustomMessageHandler('background-color', function(color) {

  document.getElementsByClassName('shiny-html-output')[0].style.backgroundColor = color;
  document.getElementsByClassName('shiny-html-output')[0].style.color = color;
});


Shiny.addCustomMessageHandler('background-color2', function(color2) {
  document.getElementsByClassName('main-section__black')[0].style.backgroundColor = color2;
  document.getElementsByClassName('main-section__black')[0].style.color = color2;

});
      
    
    