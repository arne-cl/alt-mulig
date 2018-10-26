const confpath = 'docker-compose.yml';

// Simplest way to create an RSTWorkbench instance:
// let wb = await RSTWorkbench.fromConfigFile();
class RSTWorkbench {
    // configObject: docker-compose file parsed into an Object
    // rstparsers: Array of {name: string, format: string, port: number}}
    constructor(configObject, rstparsers) {
        this.config = configObject
        this.rstparsers = rstparsers
    }

    // fromConfigFile creates a Promise(RSTWorkbench) from a
    // docker-compose config file.
    static async fromConfigFile(filepath = confpath) {
        const config = await loadConfig(filepath);
        const rstparsers = getRSTParsers(config);
        return new RSTWorkbench(config, rstparsers);
    }
}

// loadConfig loads a YAML config file from the given path and returns
// a Promise(Object) representing the config file.
async function loadConfig(filepath) {
    const res = await fetch(filepath);
    const text = await res.text();
    return jsyaml.safeLoad(text);
}

// getRSTParsers returns the metadata of all RST parsers from the object
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

// getPort returns a Port number given a service Object.
// service = {build: Object, image: string, ports: Array(string)}
function getPort(service) {
    portString = service.ports[0].split(':')[0];
    return Number(portString);
}

