

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

  document.getElementsByClassName('shiny-html-output2')[0].style.backgroundColor = color;

});



Shiny.addCustomMessageHandler('background-type', function(color) {

  
  document.getElementsByClassName('typecard')[0].style.backgroundColor = color;
  document.getElementById('sel1-label').style.color = color;
  document.getElementById('sel1-label').style.color = color;
  document.getElementById('sel1-label').style.fontSize = "200%";  

});


Shiny.addCustomMessageHandler('background-type2', function(color) {

  
  //document.getElementById('type2card').style.backgroundColor = color;
  document.getElementsByClassName('typecard')[1].style.backgroundColor = color;
  

});
      
Shiny.addCustomMessageHandler('hide-type2', function(color) {

  
  //document.getElementById('type2card').style.backgroundColor = color;
  //document.getElementsByClassName('typecard')[1].style.display = "none";
  document.getElementById('type2card').style.display = "none";

  

});


    
    