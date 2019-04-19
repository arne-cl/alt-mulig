
# coding: utf-8

# # Willkommen Österreich

# In[1]:

import collections
import os
import re

from feedgen.feed import FeedGenerator
from lxml import etree

BASE_URL = "http://willkommen-oesterreich.tv/"
EPISODES_SCRAPING_URL = os.path.join(BASE_URL, "pl.php")
PLAYLIST_BASE_URL = os.path.join(BASE_URL, 'playlists')

Episode = collections.namedtuple('Episode', 'num page date description videos playlist')


# In[2]:

def get_episode_videos(episode_id, playlist_base_url=PLAYLIST_BASE_URL):
    tree = etree.parse(os.path.join(playlist_base_url, "{}.xml".format(episode_id)))
    for episode_part in tree.iter("{http://search.yahoo.com/mrss/}content"):
        yield episode_part.attrib['url']


# In[3]:

def get_episode_id(episode_url):
    return int(re.search('pl\.php\?plid=(\d+)#', episode_url).groups()[0])

def get_episodes(episodes_scraping_url=EPISODES_SCRAPING_URL,
                 base_url=BASE_URL,
                 playlist_base_url=PLAYLIST_BASE_URL):
    html_parser = etree.HTMLParser()
    tree = etree.parse(episodes_scraping_url, parser=html_parser)

    for i, td in enumerate(tree.xpath("//td[@style='color: white; text-align:left; padding:2px; width:880px; overflow:hidden;']")):
        a1, a2 = td.getchildren()
        episode_page = os.path.join(base_url, a1.attrib['href'])
        episode_id = get_episode_id(episode_page)
        episode_date = a1.xpath('(./font/font)[2]')[0].text.strip()
        episode_description = a2.text
        episode_videos = get_episode_videos(episode_id)
        episode_playlist = os.path.join(playlist_base_url, "{}.xml".format(episode_id))
        yield Episode(num=episode_id,
                      page=episode_page, date=episode_date,
                      description=episode_description,
                      videos=episode_videos,
                      playlist=episode_playlist)


# In[9]:

def create_feed(episodes, output_filepath=None):
    woe_feed = FeedGenerator()
    woe_feed.load_extension('podcast', atom=True)
    woe_feed.title(u"Willkommen Österreich")
    woe_feed.id(EPISODES_SCRAPING_URL)
    woe_feed.link(href=BASE_URL, rel='self')
    woe_feed.description(u"Inoffizieller RSS-Feed für 'Willkommen Österreich'-Episoden")
    woe_feed.language('de')
    
    for episode in episodes:
        episode_entry = woe_feed.add_entry()
        episode_entry.id(episode.page)
        episode_entry.link(href=episode.page, rel='alternate')
        episode_entry.title(u"Folge {0} - {1}: {2}".format(episode.num, episode.date, episode.description))
        for video in episode.videos:
            episode_entry.enclosure(url=video, length=0, type='mp4')
    
    if output_filepath:
        woe_feed.atom_file(output_filepath)
    else:
        print(woe_feed.atom_str(pretty=True))


# In[10]:

episodes = get_episodes()
import itertools

last_five_episodes = itertools.islice(episodes, 5)

# woe_feed = FeedGenerator()
# woe_feed.load_extension('podcast', atom=True)
# woe_feed.title(u"Willkommen Österreich")
# woe_feed.id(EPISODES_SCRAPING_URL)
# woe_feed.link(href=BASE_URL, rel='self')
# woe_feed.description(u"Inoffizieller RSS-Feed für 'Willkommen Österreich'-Episoden")
# woe_feed.language('de')


# In[6]:

# for episode in last_five_episodes:
#     episode_entry = woe_feed.add_entry()
#     episode_entry.id(episode.page)
#     episode_entry.link(href=episode.page, rel='alternate')
#     episode_entry.title(u"Folge {0} - {1}: {2}".format(episode.num, episode.date, episode.description))
#     for video in episode.videos:
#         episode_entry.enclosure(url=video, length=0, type='mp4')


# In[11]:

create_feed(last_five_episodes)


# In[ ]:



