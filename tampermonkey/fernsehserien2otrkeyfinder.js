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

    // Replace a channel name if it has a different name on OKF than on FS
    function mapChannelName(fsChannelName) {
        const channelMap = {
            "Das Erste": "ard",
            // add more mappings here as needed
        };

        // if a mapping for the channel name exists, return it, else return the original name
        return channelMap[fsChannelName] || fsChannelName;
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

    // extract start time and duration from otrkey filename
function extractTimeAndDuration(url) {
    console.log("URL passed to extractTimeAndDuration: ", url);

    // Extract the filename from the URL's search parameters
    let searchParams = new URL(url).searchParams;
    let filename = searchParams.get('search');
    console.log("Filename: ", filename);

    // Extract time and duration from filename using a regular expression
    // The regular expression is looking for the last instance of _HH-MM_ and _XX_ where:
    // HH-MM represents the start time and XX represents the duration.
    let match = filename.match(/.*_(\d{2}-\d{2})_.*_(\d+)_/);
    if (match) {
        let startTime = match[1].replace('-', '');
        let duration = match[2];
        console.log("Start time: ", startTime, ", Duration: ", duration);
        return { startTime, duration };
    } else {
        console.warn("Failed to extract time and duration from URL: ", url);
        return null;
    }
}

    // Given the start time and duration of a recording, calculate the end time.
    function calculateEndTime(timeAndDuration) {
        if (!timeAndDuration) return null;

        // Extract the hours and minutes from the start time
        let startHour = parseInt(timeAndDuration.startTime.slice(0, 2));
        let startMinute = parseInt(timeAndDuration.startTime.slice(2, 4));

        // Convert the duration to an integer and add it to the start time
        let duration = parseInt(timeAndDuration.duration);
        let endHour = startHour + Math.floor((startMinute + duration) / 60);
        let endMinute = (startMinute + duration) % 60;

        // If endHour is 24 or more, subtract 24 from it
        endHour = endHour >= 24 ? endHour - 24 : endHour;

        // Format the end time
        let endTime = (endHour < 10 ? '0' : '') + endHour.toString() + (endMinute < 10 ? '0' : '') + endMinute.toString();

        return endTime;
    }

    // Function to create a cell element
    function createCell(text) {
        let cell = document.createElement('span');
        cell.setAttribute('role', 'cell');
        cell.innerText = text || '';  // if text is undefined, set it to an empty string
        return cell;
    }

    // Function to create a row of cells for a result
    function createResultRow(result) {
        // Extract time and duration from the filename
        let timeAndDuration = extractTimeAndDuration(result.url);
        let linkTitle;
        let filename = result.url.substring(result.url.lastIndexOf("/") + 1);
        filename = filename.replace('?search=', '');

        // Calculate the end time if timeAndDuration is not null
        if (timeAndDuration) {
            let endTime = calculateEndTime(timeAndDuration);
            let startTimeFormatted = timeAndDuration.startTime.slice(0, 2) + ":" + timeAndDuration.startTime.slice(2, 4);
            let endTimeFormatted = endTime.slice(0, 2) + ":" + endTime.slice(2, 4);
            linkTitle = startTimeFormatted + "-" + endTimeFormatted;
        } else {
            // Use the filename as the link title if timeAndDuration is null
            linkTitle = decodeURIComponent(filename);
        }

        let resultsRow = document.createElement('div');
        resultsRow.setAttribute('role', 'row');

        // Create cells
        let emptyCell = createCell('');
        let qualityCell = createCell(result.quality);
        let sizeCell = createCell(result.size);
        let mirrorsCell = createCell(result.mirrors === 1 ? '1 mirror' : result.mirrors + ' mirrors');
        
        // Create link to the otrkey file
        let linkCell = createCell();
        let link = document.createElement('a');
        link.href = result.url;
        link.innerText = linkTitle;
        
        // Style the link to make it distinguishable
        link.style.color = 'blue';
        link.style.textDecoration = 'underline';
        link.style.fontWeight = 'bold';
        
        linkCell.appendChild(link);

        resultsRow.append(emptyCell, qualityCell, sizeCell, linkCell, mirrorsCell);
        return resultsRow;
    }

    // Function to create a row with a no results message
    function createNoResultsRow() {
        let resultsRow = document.createElement('div');
        resultsRow.setAttribute('role', 'row');

        let emptyCells = Array.from({length: 4}, () => createCell(''));
        let messageCell = createCell('No OTR recordings found');

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
        let formattedChannelName = formatChannelName(mapChannelName(channelName));

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
