// ==UserScript==
// @name         FS to OKF
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Adds a "search OKF" button next to each air date on the FS page.
// @author       OpenAI
// @match        https://www.fernsehserien.de/*
// @grant        GM_xmlhttpRequest
// ==/UserScript==

(function() {
    'use strict';

    // Function to format show name, removing special characters and converting umlauts
    function formatShowName(name) {
        name = name.replace(/ä/g, 'ae').replace(/ö/g, 'oe').replace(/ü/g, 'ue').replace(/ß/g, 'ss');
        return name.replace(/[^\w\s]/gi, '').replace(/\s+/g, '%20').toLowerCase();
    }

    // Function to format channel name, removing special characters and spaces
    function formatChannelName(name) {
        return name.replace(/[^\w\s]/gi, '').replace(/\s+/g, '').toLowerCase();
    }

    // Function to convert date from 'DD.MM.YYYY' to 'YY.MM.DD'
    function convertDate(date) {
        let parts = date.split('-');
        return parts[0].slice(-2) + '.' + parts[1] + '.' + parts[2];
    }

    // Function to create the OKF search URL
    function createSearchURL(date, showName, channelName) {
        let formattedDate = convertDate(date);
        return 'https://otrkeyfinder.com/en/?search=' + showName + '%20' + formattedDate + '%20' + channelName;
    }

    // Function to download the HTML of a given URL
    function fetchHTML(url) {
        return new Promise((resolve, reject) => {
            GM_xmlhttpRequest({
                method: "GET",
                url: url,
                onload: function(response) {
                    console.log("Response text:", response.responseText);
                    if (response.status >= 200 && response.status < 300) {
                        resolve(response.responseText);
                    } else {
                        reject(new Error("Error status: " + response.status));
                    }
                },
                onerror: function(err) {
                    console.log("Error occurred during GM_xmlhttpRequest: ", err);
                    reject(err);
                }
            });
        }).catch(err => console.warn('Something went wrong.', err));
    }

    // Function to parse the results from the OKF page
    function parseResults(responseText) {
        console.log("Response text passed to parseResults: ", responseText);
        let parser = new DOMParser();
        let doc = parser.parseFromString(responseText, 'text/html');
        let otrkeys = doc.querySelectorAll('.otrkey > a.otrkey');
        console.log("otrkeys elements found: ", otrkeys);

        let results = Array.from(otrkeys).map(otrkey => {
            console.log("otrkey element: ", otrkey);

            let quality = otrkey.querySelector('.format') ? otrkey.querySelector('.format').innerText : null;
            let mirrors = otrkey.querySelector('.mirrors') ? otrkey.querySelector('.mirrors').innerText : null;
            let size = otrkey.parentElement.querySelector('.otrkey-info > .img-ico-info') ? otrkey.parentElement.querySelector('.otrkey-info > .img-ico-info').innerText.split(": ")[1] : null;

            console.log(".quality element: ", quality);
            console.log(".size element: ", size);
            console.log(".mirrors element: ", mirrors);

            // Build the correct URL with OKF domain
            let url = 'https://www.otrkeyfinder.com' + otrkey.getAttribute('href');

            return {
                url: url,
                quality: quality,
                size: size,
                mirrors: mirrors,
            };
        });
        return results;
    }


    // Function to create a cell element
    function createCell(className, text) {
        let cell = document.createElement('span');
        cell.setAttribute('role', 'cell');
        cell.className = className;
        cell.innerText = text || '';  // if text is undefined, set it to an empty string
        return cell;
    }

    // Function to create a row of cells for a result
    function createResultRow(result) {
        let resultsRow = document.createElement('div');
        resultsRow.setAttribute('role', 'row');

        let qualityCell = createCell('sendetermine-2019-wochentag okf-quality', result.quality);
        let sizeCell = createCell('sendetermine-2019-datum okf-file-size', result.size);
        let mirrorsCell = createCell('sendetermine-2019-uhrzeit okf-mirrors', 
                                     result.mirrors === 1 ? '1 mirror' : result.mirrors + ' mirrors');
        let senderCell = createCell('sendetermine-2019-sender');
        let titleCell = createCell('sendetermine-2019-episodentitel');

        let link = document.createElement('a');
        link.href = result.url;
        let filename = result.url.substring(result.url.lastIndexOf("/") + 1);
        filename = filename.replace('?search=', '');
        link.innerText = decodeURIComponent(filename);
        titleCell.appendChild(link);

        resultsRow.append(qualityCell, sizeCell, mirrorsCell, senderCell, titleCell);
        return resultsRow;
    }

    // Function to create a row with a no results message
    function createNoResultsRow() {
        let resultsRow = document.createElement('div');
        resultsRow.setAttribute('role', 'row');

        let emptyCells = Array.from({length: 4}, () => createCell('sendetermine-2019-streams'));
        let messageCell = createCell('sendetermine-2019-episodentitel', 'No OTR recordings found');

        resultsRow.append(...emptyCells, messageCell);
        return resultsRow;
    }

    // Function to display the results under a row
    function displayResults(row, results) {
        if(results.length === 0){
            let resultsRow = createNoResultsRow();
            row.insertAdjacentElement('afterend', resultsRow);
        } else {
            results.forEach(result => {
                let resultsRow = createResultRow(result);
                row.insertAdjacentElement('afterend', resultsRow);
            });
        }
    }



    // Get show name and format it
    let showName = document.querySelector('.serien-titel > a:nth-child(1)').innerText;
    let formattedShowName = formatShowName(showName);
    console.log('Show name:', showName, ', formatted:', formattedShowName);

    // Get all rows with air dates and iterate over them
    let rows = document.querySelectorAll('#episode-sendetermine > div');
    console.log('Found', rows.length, 'rows');
    rows.forEach(row => {
        // Extract date, time, and channel name
        let date = row.querySelector('span.sendetermine-2019-datum > time').getAttribute('datetime').slice(0,10);
        let channelName = row.querySelector('span.sendetermine-2019-sender > span').getAttribute('content');
        let formattedChannelName = formatChannelName(channelName);

        console.log('Row:', row);
        console.log('Date:', date);
        console.log('Channel name:', channelName, ', formatted:', formattedChannelName);

        // Create the OKF search URL
        let searchURL = createSearchURL(date, formattedShowName, formattedChannelName);
        console.log('Search URL:', searchURL);

        // Create "search OKF" button and append it to the row
        let button = document.createElement('a');
        button.style.backgroundColor = '#4CAF50';
        button.style.border = 'none';
        button.style.color = 'white';
        button.style.padding = '5px 10px';
        button.style.textAlign = 'center';
        button.style.textDecoration = 'none';
        button.style.display = 'inline-block';
        button.style.fontSize = '16px';
        button.style.margin = '4px 2px';
        button.style.cursor = 'pointer';

        button.href = '#';
        button.onclick = function(event) {
            event.preventDefault();
            fetchHTML(searchURL).then(html => {
                console.log("Fetched HTML: ", html);
                // Parse the HTML, extract the results, and display them under the air date
                let results = parseResults(html);
                console.log("Parsed results: ", results);
                displayResults(row, results);
            });
        };
        button.innerText = 'search OKF';
        console.log('Button created:', button);

        // Create "search OKF in new tab" button and append it to the row
        let newTabButton = document.createElement('a');
        // Copy styles from the original button
        newTabButton.style = button.style.cssText;

        newTabButton.href = searchURL;  // Link directly to the search URL
        newTabButton.target = '_blank';  // Open in new tab
        newTabButton.innerText = 'search OKF in new tab';
        console.log('New tab button created:', newTabButton);

        row.appendChild(button);
        row.appendChild(newTabButton);  // Append the new button to the row
        console.log('Buttons appended to row');
    });
})();
