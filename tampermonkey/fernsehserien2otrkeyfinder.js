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

            return {
                url: otrkey.href,
                quality: quality,
                size: size,
                mirrors: mirrors,
            };
        });
        return results;
    }


    // Function to display the results under a row
    function displayResults(row, results) {
        // Check if there are any results
        if (results.length === 0) {
            // Create new row for the no results message
            let resultsRow = document.createElement('div');
            resultsRow.setAttribute('role', 'row');

            // Create and add empty cells to the resultsRow
            for (let i = 0; i < 5; i++) {
                let cell = document.createElement('span');
                cell.setAttribute('role', 'cell');
                if (i === 4) {
                    cell.className = 'sendetermine-2019-episodentitel';
                    cell.innerText = 'No OTR recordings found';
                } else {
                    cell.className = 'sendetermine-2019-streams';
                }
                resultsRow.appendChild(cell);
            }

            // Insert the resultsRow after the row
            row.insertAdjacentElement('afterend', resultsRow);
        } else {
            results.forEach(result => {
                // Create new row for the result
                let resultsRow = document.createElement('div');
                resultsRow.setAttribute('role', 'row');

                // Create and add cells to the resultsRow
                let cell1 = document.createElement('span');
                cell1.setAttribute('role', 'cell');
                cell1.className = 'sendetermine-2019-streams';

                let cell2 = document.createElement('span');
                cell2.setAttribute('role', 'cell');
                cell2.className = 'sendetermine-2019-wochentag okf-quality';
                cell2.innerText = result.quality;

                let cell3 = document.createElement('span');
                cell3.setAttribute('role', 'cell');
                cell3.className = 'sendetermine-2019-datum okf-file-size';
                cell3.innerText = result.size;

                let cell4 = document.createElement('span');
                cell4.setAttribute('role', 'cell');
                cell4.className = 'sendetermine-2019-uhrzeit okf-mirrors';
                cell4.innerText = result.mirrors === 1 ? '1 mirror' : result.mirrors + ' mirrors';

                let cell5 = document.createElement('span');
                cell5.setAttribute('role', 'cell');
                cell5.className = 'sendetermine-2019-sender';

                let cell6 = document.createElement('span');
                cell6.setAttribute('role', 'cell');
                cell6.className = 'sendetermine-2019-episodentitel';

                let link = document.createElement('a');
                link.href = result.url;

                // Extract filename from URL, remove the "?search=" prefix and use it as the link text
                let filename = result.url.substring(result.url.lastIndexOf("/") + 1);
                filename = filename.replace('?search=', '');
                link.innerText = decodeURIComponent(filename);

                cell6.appendChild(link);

                // Add button that links to the OKF results page
                let okfButton = document.createElement('button');
                okfButton.innerText = 'open OKF page';
                okfButton.addEventListener('click', function() {
                    window.open(result.okfPageUrl, '_blank');
                });

                resultsRow.append(cell1, cell2, cell3, cell4, cell5, cell6, okfButton);

                // Insert the resultsRow after the row
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

        row.appendChild(button);
        console.log('Button appended to row');
    });
})();
