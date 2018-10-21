"use strict";

// NOTE: loading a local file will only work if we serve this directory
// from a webserver, even if it's just "python -m SimpleHTTPServer".
$(document).ready(function () {
  parseYAML('docker-compose.yml');
});


function parseYAML(yamlFilepath) {
    $.get(yamlFilepath)
    .done(function (data) {
        var yamlObject = jsyaml.safeLoad(data);
        $("#results").append(yamlObject);
    });
}

function addToResults(thing) {
    $("#results").append(thing + '<br>\n');
}

//~ Object.keys(yamlObject['services']).forEach(addToResults)
//~ yamlObject['services']['codra-service']['build']['labels']['type']
