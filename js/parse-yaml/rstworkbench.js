// TODO: import jsyaml directly, so we don't need it in vanilla.html

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
    
    async getParseResults(text) {
        let results = [];
        for (let parser of this.rstparsers) {
            try {
                const response = await parser.parse(text);
                const output = await response.text();
                addToResults(parser.name, output);
            } catch(e) {
                addToErrors(parser.name, e);
            }
        }
    }
}

// addToElement adds a title and content to the given DOM element, e.g.
// <div id=$elementID>
//   <div>
//     <h2>$title</h2>
//     <p>$content</p>
//   </div>
// </div>
function addToElement(elementID, title, content) {
    let resultsElem = document.getElementById(elementID);
    
    let resultElem = document.createElement('div');
    let titleElem = document.createElement('h2');
    titleElem.innerText = title;
    
    let contentElem = document.createElement('p');
    contentElem.innerText = content;
    
    resultElem.appendChild(titleElem);
    resultElem.appendChild(contentElem);
    resultsElem.appendChild(resultElem);
}

// addToResults adds a title (e.g. the name of a parser) and some content
// to the results section of the page.
function addToResults(title, content) {
    addToElement('results', title, content);
}

// addToErrors adds a title (e.g. the name of the parser that produced
// the error) and and error message to the  section of the page.
function addToErrors(title, error) {
    addToElement('errors', title, error.message);
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
    let parsers = [];
    for (let serviceKey of Object.keys(yamlObject.services)) {
        service = yamlObject.services[serviceKey]
        serviceLabel = service.build.labels
        if (serviceLabel.type === 'rst-parser' ) {
            //~ let parserConfig = {
                //~ name: serviceLabel.name,
                //~ format: serviceLabel.format,
                //~ port: getPort(service)
            //~ };
            
            let parser = new RSTParser(
                serviceLabel.name,
                serviceLabel.format,
                getPort(service));
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


class RSTParser {
    constructor(name, format, port) {
        this.name = name
        this.format = format
        this.port = port
    }

    // TODO: add GET /status to all parser APIs
    async isRunning() {
        let running = false;
        
        try {
            const response = await fetch(`http://localhost:${this.port}/status`);
            if (response.ok && response.status == 200) {
                running = true;
            }
        } catch(e) {
            console.log(e.message);
        }
        return running;
    }

    async parse(input) {
        const data = new FormData();
        data.append('input', input);
        data.append('output_format', 'original'); // FIXME: rm after cleanup of parser APIs

        const options = {
          method: 'POST',
          body: data,
        };

        const response = await fetch(`http://localhost:${this.port}/parse`, options);
        const output = await response.text();
        return output
    }
}
