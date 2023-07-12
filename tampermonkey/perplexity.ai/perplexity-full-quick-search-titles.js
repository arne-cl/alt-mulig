// ==UserScript==
// @name         Show full title of web pages in perplexity.ai search results
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  perplexity.ai not only answers your questions, but also links to its sources.\nUnfortunately, it only shows abbreviated titles of the website.\nThis userscript fixes that.
// @author       GPT-4 (coding), Arne Neumann (prompting)
// @match        https://www.perplexity.ai/*
// @grant        none
// ==/UserScript==

(function() {
    'use strict';

    // Function to remove a specific class from divs with that class
    function removeClassFromDivs(className) {
        const divs = document.getElementsByClassName(className);
        while (divs[0]) {
            divs[0].classList.remove(className);
        }
    }

    // Remove the line-clamp-1 class from all divs initially.
    // Removing it will show full website titles for the "Quick Search" results
    // instead of "..."-abbreviated titles.
    removeClassFromDivs('line-clamp-1');

    // Create a mutation observer to watch for changes in the DOM
    const observer = new MutationObserver(function(mutations) {
        // If new nodes are added
        if (mutations.some(mutation => mutation.addedNodes.length > 0)) {
            // Remove the line-clamp-1 class from new divs
            removeClassFromDivs('line-clamp-1');
        }
    });

    // Start observing the document with the configured parameters
    observer.observe(document, { childList: true, subtree: true });
})();
