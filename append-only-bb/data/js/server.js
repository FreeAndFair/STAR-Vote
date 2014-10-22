function step1recomp(event) {
    var json = JSON.stringify({
        'command': 'sign-timestamp',
        'message': $('#message').val(),
        'timestamp': $('#timestamp').val(),
        'author': $('#author').val(),
        'oldhash': $('#the-hash').attr('data-hash')
    });
    $('#sigInput').val(json);
}

function registerrecomp(event) {
    var json = JSON.stringify({
        'command': 'keygen',
        'name': $('#registerName').val()
    });
    $('#regInput').val(json);
}

$(document).ready(function(){
    $('.step1').change(step1recomp);
    step1recomp();

    $('#registerName').change(registerrecomp);
    registerrecomp();

});
