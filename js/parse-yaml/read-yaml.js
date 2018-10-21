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
        for (var parser of rstParsers) {
            addToResults(JSON.stringify(parser));
        }
    });
}

// getRSTParsers returns the names of all RST parsers from the object
// representation of a docker-compose.yml file.
function getRSTParsers(yamlObject) {
    var parsers = [];
    for (var serviceKey of Object.keys(yamlObject.services)) {
        service = yamlObject.services[serviceKey]
        serviceLabel = service.build.labels
        if (serviceLabel.type === 'rst-parser' ) {
            parser = {
                name: serviceLabel.name,
                format: serviceLabel.format,
                port: getPort(service)
            };
            parsers.push(parser);
        }
    }
    return parsers;
}

function getPort(service) {
    portString = service.ports[0].split(':')[0];
    return Number(portString);
}

function addToResults(thing) {
    $("#results").append(thing + '<br><br>\n');
}
