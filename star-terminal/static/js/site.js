(function() {

  function getRaceKey() {
    var inputs = document.getElementsByName("race-key");
    return inputs[0].value;
  }

  function getSelection() {
    var options = document.getElementsByClassName('ballot-option');
    options = Array.prototype.slice.call(options);  // converts HTMLCollection to Array
    var checked = options.filter(function(input) { return input.checked; });
    return checked.map(function(input) { return input.value; });
  }

  function record(raceKey, value, secure) {
    var kv = encodeURIComponent(raceKey) + "=" + encodeURIComponent(value);
    var path = ";path=" + window.location.pathname.split("/").slice(0,3).join("/");
    var sec  = secure ? ";secure" : "";
    document.cookie = kv + path + sec;
  }

  function highlightButton() {
    var button = document.querySelectorAll(".navbar-right")[0];
    if(button.className.match(/\bactivated\b/) === null)
      button.className += " activated";
  }

  document.addEventListener('change', function(event) {
    var raceKey = getRaceKey();
    getSelection().forEach(function(value) {
      record(raceKey, value);
      highlightButton();
    });
  }, false);

}());
