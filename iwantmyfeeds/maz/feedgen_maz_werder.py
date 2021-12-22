import logging

import feedgen
from feedgen.feed import FeedGenerator
import requests_html
from requests_html import HTMLSession


BASE_URL = 'https://www.maz-online.de/Lokales/Potsdam-Mittelmark/Werder-Havel'


def get_maz_articles(session, url):
    r = session.get(url)
    articles = []
    for pub_type in ('articles4', 'teaser3'):
        for article in r.html.find(f'div.pdb-{pub_type}-teaser'):
            try:
                link_elem = article.find(f'a.pdb-{pub_type}-teaser-breadcrumb-headline-title-link', first=True)
                link_abs_url = requests_html.urljoin(BASE_URL, link_elem.attrs['href'])

                pub_date_elem = article.find(f'span.pdb-{pub_type}-teaser-release-publicationdate', first=True)
                pub_date = ''
                if pub_date_elem:
                    pub_date = pub_date_elem.text

                headline = article.find(f'a.pdb-{pub_type}-teaser-breadcrumb-headline-title-link', first=True).text
                img = article.find('img', first=True)
                intro = article.find('p', first=True).text

                if pub_date:  # articles w/out a publication date are spam/listicles
                    article = {
                        'url': link_abs_url,
                        'pub_date': pub_date,
                        'headline': headline,
                        'intro': intro,
                        'image_url': img.attrs['src'],
                        'alt_text': img.attrs['alt']}
                    articles.append(article)
            except Exception as ex:
                logging.exception('Caught exception when parsing MAZ/Werder page.')
    return articles

def create_feed(articles, output_filepath=None):
    feed = FeedGenerator()
    feed.title(u"MAZ: Werder/Havel")
    feed.id(BASE_URL)
    feed.link(href='https://www.maz-online.de/', rel='self')
    feed.description(u"Inoffizieller RSS-Feed für MAZ Werder/Havel")
    feed.language('de')
    
    for article in articles:
        entry = feed.add_entry()
        entry.link(href=article['url'], rel='alternate')
        entry.id(article['url'])
        entry.title(article['headline'])
        entry.content(f"""<img src="{article['image_url']}" align="left" hspace="10"> {article["pub_date"]} {article["intro"]}""")
        
    if output_filepath:
        feed.atom_file(output_filepath)
    else:
        print(feed.atom_str(pretty=True).decode('utf-8'))


if __name__ == '__main__':
    session = HTMLSession()
    articles = get_maz_articles(session, BASE_URL)
    create_feed(articles)
