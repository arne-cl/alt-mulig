// NOTE: loading a local file will only work if we serve this directory
// from a webserver, even if it's just "python -m SimpleHTTPServer".
$(document).ready(function () {
  parseYAML('docker-compose.yml');
});

function parseYAML(filepath) {
    $.get(filepath)
    .done(function (data) {
        var yamlObject = jsyaml.safeLoad(data);
        var jsonString = JSON.stringify(data);
        var jsonObject = $.parseJSON(jsonString);
        addToResults(jsonObject);

        var rstParsers = getRSTParsers(yamlObject);
        addToResults(rstParsers);
    });
}

// getRSTParsers returns the names of all RST parsers from the object
// representation of a docker-compose.yml file.
function getRSTParsers(yamlObject) {
    var parsers = [];
    for (var service of Object.keys(yamlObject.services)) {
        serviceType = yamlObject.services[service].build.labels.type
        if (serviceType === 'rst-parser' ) {
            parsers.push(service);
        }
    }
    return parsers;
}

function addToResults(thing) {
    $("#results").append(thing + '<br><br>\n');
}

//~ Object.keys(yamlObject['services']).forEach(addToResults)
//~ yamlObject['services']['codra-service']['build']['labels']['type']
