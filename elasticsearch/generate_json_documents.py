#!/usr/bin/env python
# -*- coding: utf-8 -*-
# original author: Marko Drotschmann
# changes: Arne Neumann

import json
import hashlib

'''
# an example document for a 'celebrity'

{
# Alter
"age": ,
# ausgedachtes Rating von 1.0 bis 5.0
"rating": 0.0,
# Geburtstag, Format yyyy-mm-dd
"birthday": "",
# Vorname
"firstname": "",
# Nachname
"lastname": "",
# voller Name, inklusive Zwischennamen und eventueller Adels- oder
Doktortitel
"fullname": "",
# die Kategorien, die bei der Wikipedia unten dem Prominenten
zugeordnet sind (z.B. Mann, Frau, Komiker, Mitglied des Bundestages, etc.)
"categories": [
],
# eine Stadt, die mit dem Prominenten verbunden ist, z.B.
Geburtsstadt, derzeitiger Wohnort, etc.
"city": "",
# Koordinaten
"coordinates": {
"lat": ,
"lon": 
},
# der erste Absatz mit einführenden Worten zu dem Prominenten
"teaser": "",
# entweder der gesamte Artikel oder nur der Lebenslaufteil aus der
#Wikipedia kopiert
"vita": ""
}

'''

steve_martin_data = {
# Alter
"age": 69,
# ausgedachtes Rating von 1.0 bis 5.0
"rating": 4.2,
# Geburtstag, Format yyyy-mm-dd
"birthday": "1945-08-14",
# Vorname
"firstname": "Steve",
# Nachname
"lastname": "Martin",
# voller Name, inklusive Zwischennamen und eventueller Adels- oder
#Doktortitel
"fullname": "Stephen Glenn Martin",
# die Kategorien, die bei der Wikipedia unten dem Prominenten
#zugeordnet sind (z.B. Mann, Frau, Komiker, Mitglied des Bundestages, etc.)
"categories": [
    "Schauspieler",
    "Filmproduzent",
    "Komiker",
    "Autor",
    "Literatur (20. Jahrhundert)",
    "Literatur (Englisch)",
    "US-amerikanischer Musiker",
    "Emmy-Preisträger",
    "Grammy-Preisträger",
    "Oscarpreisträger",
    "Mitglied der American Academy of Arts and Sciences",
    "US-Amerikaner",
    "Geboren 1945",
    "Mann",
],
# eine Stadt, die mit dem Prominenten verbunden ist, z.B.
#Geburtsstadt, derzeitiger Wohnort, etc.
"city": "Waco",
# Koordinaten
"coordinates": {
"lat": 31.551388888889,
"lon": 97.155833333333
},
# der erste Absatz mit einführenden Worten zu dem Prominenten
"teaser": 
"""Steve Martin (eigentlich: Stephen Glenn Martin; * 14. August 1945 in Waco,
Texas) ist ein US-amerikanischer Komiker, Schriftsteller, Musiker, Produzent
und Schauspieler. 2013 wurde ihm der Ehrenoscar verliehen.""",
# entweder der gesamte Artikel oder nur der Lebenslaufteil aus der
#Wikipedia kopiert
"vita": 
"""
Steve Martin (eigentlich: Stephen Glenn Martin; * 14. August 1945 in Waco,
Texas) ist ein US-amerikanischer Komiker, Schriftsteller, Musiker, Produzent
und Schauspieler. 2013 wurde ihm der Ehrenoscar verliehen.

Leben

Nach dem Abschluss der Garden Grove High School im Jahr 1963 studierte Martin
an der California State University einige Semester Philosophie und
Theaterwissenschaften. Nebenbei arbeitete er im Magic Shop von Disneyland, wo
er seine Fähigkeiten im Jonglieren, Zaubern, Banjospielen und Ballontiereformen
entwickelte. Ende der 1960er schrieb er sein erstes Bühnenprogramm, mit dem er
durch zahlreiche kleinere Clubs in Los Angeles tourte.

Martin machte sich in der Branche schnell einen Namen. Bereits 1969 gewann er
als Autor der Smothers Brothers Comedy Hour einen Emmy. 1971 war er festes
Ensemblemitglied der Sonny & Cher-Show. Mit zahlreichen Auftritten in der
Tonight Show mit Johnny Carson wurde er einem breiten Publikum bekannt. Dass er
in dem Bruce-Lee-Film Todesgrüße aus Shanghai sein Leinwanddebüt gegeben haben
soll, ist eine verbreitete Legende. Tatsächlich sieht ihm ein Polizist in
diesem Film nur sehr ähnlich.

1976 moderierte er erstmals die legendäre Comedy-Sendung Saturday Night Live,
für die er in den folgenden zehn Jahren u.a. mit Dan Aykroyd, John Belushi,
Chevy Chase, Eddie Murphy, Bill Murray und Martin Short vor der Kamera stand.

In dem Film Reichtum ist keine Schande, zu dem er auch das Drehbuch schrieb,
spielte Martin 1979 seine erste Hauptrolle. Für den Kurzfilm The Absent-Minded
Waiter erhielt er im selben Jahr eine Oscar-Nominierung. In den 1980er Jahren
war Martin u.a. in der Film-noir-Parodie Tote tragen keine Karos, den jeweils
Golden-Globe-nominierten Komödien Solo für zwei und Roxanne sowie dem
Oscar-nominierten Horror-Musical Der kleine Horrorladen zu sehen. Mit diesen
Filmen wurde er nun auch in Europa bekannt.

1991 wirkte er in L.A. Story und Vater der Braut, einem Remake des
Spencer-Tracy-Klassikers, mit. Das Drama Grand Canyon – Im Herzen der Stadt, in
dem er neben Kevin Kline und Danny Glover die Hauptrolle spielte, gewann auf
der Berlinale den Goldenen Bären. Viel Beachtung fand seine Darstellung in dem
Thriller Die unsichtbare Falle, in dem er in einer seiner wenigen ernsthaften
Rollen zu sehen ist. Für Housesitter und Schlaflos in New York stand Martin
1992 und 1999 gemeinsam mit Goldie Hawn vor der Kamera.

2001 und 2003 moderierte er die Oscarverleihung und wurde dafür mehrfach für
den Emmy nominiert. Auch als Autor ist Martin nach wie vor aktiv. Das
Theaterstück Picasso at the Lapin Agile wurde 1993 in Chicago uraufgeführt und
wird derzeit verfilmt. Seine Bücher, die Kurzgeschichtensammlung Pure Drivel
und der Roman Shopgirl (2005 verfilmt), waren Ende der 90er ebenfalls sehr
erfolgreich. 2003 spielte er die Rolle als Vater von zwölf Kindern in „Im
Dutzend billiger“. 2005 übernahm Steve Martin in einer Neuverfilmung von Der
rosarote Panther die Rolle des Inspektor Clouseau und spielte erneut den Vater
in „Im Dutzend billiger 2“.

2009 war er mit Der rosarote Panther 2 erneut in dieser Rolle zu sehen, zudem
an der Seite von Meryl Streep und Alec Baldwin in der Liebeskomödie It's
Complicated von Nancy Meyers. Im selben Jahr erhielt Martin für seinen
Gastauftritt als Gavin Volure in der gleichnamigen Episode der Serie 30 Rock
seine fünfte Emmy-Nominierung.

Von 1986 bis 1994 war er mit der Schauspielerin Victoria Tennant verheiratet.
Am 28. Juli 2007 heiratete er die Journalistin Anne Stringfield, mit der er
seit Dezember 2012 ein Kind hat.

Martin wurde 2007 mit dem Kennedy-Preis ausgezeichnet.

2010 erhielt er den Grammy in der Kategorie Bestes Bluegrass-Album für The Crow
/ New Songs for the Five-String Banjo. Im selben Jahr wurde er in die American
Academy of Arts and Sciences gewählt.

Am 7. März 2010 führte Martin zusammen mit Alec Baldwin im Kodak Theatre noch
einmal durch die Oscarverleihung. 2014 erhielt er den Ehrenoscar für sein
Lebenswerk.

Filmografie (Auswahl)

  • 1970: The Ray Stevens Show
  • 1971: The Sonny & Cher Comedy Hour
  • 1975: The Smothers Brothers Show
  • 1976: Johnny Cash And Friends
  • 1976: Saturday Night Live
  • 1977: The Absent-Minded Waiter (Kurzfilm)
  • 1977: The Muppet Show
  • 1978: Sgt. Pepper’s Lonely Hearts Club Band
  • 1979: Reichtum ist keine Schande (The Jerk)
  • 1981: Tanz in den Wolken (Pennies from Heaven)
  • 1982: Tote tragen keine Karos (Dead Men Don’t Wear Plaid)
  • 1983: Der Mann mit zwei Gehirnen (The Man with Two Brains)
  • 1984: Solo für 2 (All of Me)
  • 1984: Ein Single kommt selten allein (The Lonely Guy)
  • 1986: Der kleine Horrorladen (Little Shop of Horrors)
  • 1986: Drei Amigos! (¡Three Amigos!)
  • 1987: Die Tracey Ullman Show
  • 1987: Roxanne
  • 1987: Ein Ticket für Zwei (Planes, Trains & Automobiles)
  • 1988: Zwei hinreißend verdorbene Schurken (Dirty Rotten Scoundrels)
  • 1989: Eine Wahnsinnsfamilie (Parenthood)
  • 1990: Das Schlitzohr von der Mafia (My Blue Heaven)
  • 1991: Grand Canyon – Im Herzen der Stadt
  • 1991: Vater der Braut (Father of the Bride)
  • 1991: L.A. Story
  • 1992: Housesitter – Lügen haben schöne Beine (House Sitter)
  • 1992: Der Schein-Heilige (Leap of Faith)
  • 1993: … und das Leben geht weiter (And the Band Played On)
  • 1994: Der Zufalls-Dad (A Simple Twist of Fate)
  • 1994: Lifesavers – Die Lebensretter (Mixed Nuts)
  • 1995: Ein Geschenk des Himmels – Vater der Braut 2 (Father of the Bride
    Part II)
  • 1996: Immer Ärger mit Sergeant Bilko (Sgt. Bilko)
  • 1997: Die unsichtbare Falle (The Spanish Prisoner)
  • 1998: Der Prinz von Ägypten (The Prince of Egypt) – Stimme
  • 1999: Bowfingers große Nummer (Bowfinger)
  • 1999: Schlaflos in New York (The Out-of-Towners)
  • 2001: Fantasia 2000 – Sprecher
  • 2001: Novocaine – Zahn um Zahn (Novocaine)
  • 2003: Haus über Kopf (Bringing Down the House)
  • 2003: Looney Tunes: Back in Action
  • 2003: Im Dutzend billiger (Cheaper by the Dozen)
  • 2005: Shopgirl
  • 2005: Im Dutzend billiger 2 – Zwei Väter drehen durch (Cheaper by the Dozen
    2)
  • 2006: Der rosarote Panther (The Pink Panther)
  • 2008: Baby Mama
  • 2009: Der rosarote Panther 2 (The Pink Panther deux)
  • 2009: Wenn Liebe so einfach wäre (It’s Complicated)
  • 2011: Ein Jahr vogelfrei! (The Big Year)

Diskografie

  • Let’s Get Small (1977)
  • A Wild and Crazy Guy, (1978)
  • Comedy Is Not Pretty! (1979)
  • The Steve Martin Brothers (1981)
  • Born Standing Up (2007)
  • The Crow: New Songs for the 5-String Banjo (2009), Grammy
  • Rare Bird Alert (2011), zusammen mit den Steep Canyon Rangers
  • Love Has Come for You (2013), mit Edie Brickell, Grammy (für den Titelsong)

Literarische Veröffentlichungen

  • Cruel shoes. G. P. Putnam’s Sons, New York 1979, ISBN 978-0-399-12304-7
    (Kurzgeschichten)
  • Picasso at the lapin agile and other plays. Grove Press, New York 1997,
    ISBN 978-0-8021-3523-0 (Theaterstücke)
  • Sehr erfreut, meine Bekanntschaft zu machen. Manhattan 2004, ISBN
    3-442-54574-9 (mit Detlev Ullrich)
  • Shopgirl. Hyperion, New York 2001, ISBN 978-0-7868-8568-8 (Erzählung)
  • Blanker Unsinn. Goldmann, München 2002, ISBN 978-3-442-45152-4
    (Kurzgeschichten)

Sonstiges

Die deutsche Synchronstimme von Steve Martin sprechen unter anderem Norbert
Gescher und Eckart Dux.
"""
}

steve_martin = ('comedian', steve_martin_data)


bill_callahan_data = {
# Alter
"age": 49,
# ausgedachtes Rating von 1.0 bis 5.0
"rating": 5.0,
# Geburtstag, Format yyyy-mm-dd
"birthday": "1966-01-01",
# Vorname
"firstname": "Bill",
# Nachname
"lastname": "Callahan",
# voller Name, inklusive Zwischennamen und eventueller Adels- oder
#Doktortitel
"fullname": "Bill Callahan",
# die Kategorien, die bei der Wikipedia unten dem Prominenten
#zugeordnet sind (z.B. Mann, Frau, Komiker, Mitglied des Bundestages, etc.)
"categories": [
    "Alternative-Country-Musiker",
    "US-amerikanischer Musiker",
    "Geboren 1966",
    "Mann"
],
# eine Stadt, die mit dem Prominenten verbunden ist, z.B.
#Geburtsstadt, derzeitiger Wohnort, etc.
"city": "Silver Spring",
# Koordinaten
"coordinates": {
"lat": 39.004166666667,
"lon": 77.018888888889
},
# der erste Absatz mit einführenden Worten zu dem Prominenten
"teaser": 
"""
Bill Callahan (* 1966 in Silver Spring, Maryland, USA) ist ein
US-amerikanischer Sänger und Songwriter, der mit seinen anfänglich mit
einfachster Produktion auf Vier-Spur-Rekordern aufgenommenen Songs als einer
der Vorreiter des Lo-Fi gilt. Seit 1991 veröffentlicht er beim Label Drag City.
Er trat zunächst unter den Namen "Smog" bzw. "(Smog)", seit 2007 aber unter
seinem bürgerlichen Namen in Erscheinung. Callahan lebt derzeit in Austin,
Texas.
""",
# entweder der gesamte Artikel oder nur der Lebenslaufteil aus der
#Wikipedia kopiert
"vita": 
"""
Bill Callahan (* 1966 in Silver Spring, Maryland, USA) ist ein
US-amerikanischer Sänger und Songwriter, der mit seinen anfänglich mit
einfachster Produktion auf Vier-Spur-Rekordern aufgenommenen Songs als einer
der Vorreiter des Lo-Fi gilt. Seit 1991 veröffentlicht er beim Label Drag City.
Er trat zunächst unter den Namen "Smog" bzw. "(Smog)", seit 2007 aber unter
seinem bürgerlichen Namen in Erscheinung. Callahan lebt derzeit in Austin,
Texas.

Karriere

Obwohl Callahan in Maryland geboren wurde, verbrachte seine Familie acht Jahre
in Knaresborough, North Yorkshire, und kehrte bloß für die Jahre 1969-1973 nach
Maryland zurück.

Callahans erste Veröffentlichungen erschienen unter dem Bandnamen "Smog" auf
Audiocassetten. Diese waren von kargen Melodien und dissonanten Arrangements
geprägt. Damit entsprachen sie in etwa Callahans damaligen instrumentalen und
produktionstechnischen Möglichkeiten. Sein Debütalbum "Sewn to the Sky" erregte
durch verstimmte Gitarren und repetitive Strukturen erstes Aufsehen und
erinnerte an Arbeiten von Jandek oder Daniel Johnston. Mit Beginn der Arbeit
bei Drag City erweiterte sich das musikalische Potential seiner Musik, die z.
B. von John McEntire und Jim O'Rourke produziert wurde. Dabei entwickelte
Callahan nicht nur seine lyrischen, oft schwarzhumorigen Fähigkeiten, auch
seine Arrangements wurden zunächst reichhaltiger. Zwischen 2001 und 2003 nannte
er die Band "(Smog)" und kehrte zu einfacheren Produktionen zurück, ohne aber
die textliche Raffinesse, für die er mittlerweile stand, aufzugeben.

2006 entschloss er sich zur Nutzung seines bürgerlichen Namens und ließ das
Pseudonym "Smog" fallen. So erschien 2007 mit "Woke on a Whaleheart" das erste
Studioalbum als Bill Callahan, 2009 folgte Sometimes I Wish We Were an Eagle.
Im Jahr 2011 erschien das Album "Apocalypse" und 2013 "Dreamriver".

Veröffentlichungen

Alben

als Smog bzw. (Smog)

  • "Forgotten Foundation", 1992/1996
  • "Burning Kingdom", 1994
  • "Julius Caesar", 1994
  • "Wild Love", 1995
  • "Sewn to the Sky", 1995 (Wiederveröffentlichung)
  • "The Doctor Came at Dawn", 1996
  • "Red Apple Falls", 1997
  • "Knock Knock", 1999
  • "Dongs of Sevotion", 2000
  • "Rain on Lens", 2001
  • "Accumulation: None", 2002
  • "Supper", 2003
  • "A River Ain't Too Much to Love", 2005

als Bill Callahan

  • Woke on a Whaleheart, 2007
  • Sometimes I Wish We Were an Eagle, 2009
  • Apocalypse, 2011
  • Dream River, 2013
  • Have Fun with God, 2014

Andere Formate

als Smog bzw. (Smog)

  • "Macrame Gunplay" (Cassette), 1988
  • "Cow" (Cassette), 1989
  • "A Table Setting" (Cassette), 1990
  • "Tired Tape Machine" (Cassette), 1990
  • "Sewn to the Sky" (Cassette), 1990
  • "Floating" (EP), 1991
  • "A Hit" (Single), 1994
  • "Kicking a Couple Around" (EP), 1994
  • "Came Blue" (Single), 1997
  • "Ex-con" (Single), 1997
  • "Cold-Blooded Old Times" (EP), 1999
  • "Look Now" (Single), 1999
  • "Strayed" (Single), 2000
  • "'Neath the Puke Tree" (EP), 2000
  • "The Manta Rays of Time" (EP), 2000
  • "Rock Bottom Riser", 2006

als Bill Callahan

  • "Diamond Dancer" (Single), 2007
  • "Rough Travel for a Rare Thing", (Live-Album, nur Vinyl oder Download),
    2010

Bücher

Callahan veröffentlichte im Jahr 2004 drei Bücher mit Zeichnungen: Ballerina
Scratchpad, The Death's Head Drawings und Women. Im Juli 2010 veröffentlichte
Drag City auch seinen Briefroman Letters to Emma Bowlcut.
"""
}

bill_callahan = ('musician', bill_callahan_data)


jon_gnarr_data = {
# Alter
"age": 48,
# ausgedachtes Rating von 1.0 bis 5.0
"rating": 1.4,
# Geburtstag, Format yyyy-mm-dd
"birthday": "1967-01-02",
# Vorname
"firstname": "Jón",
# Nachname
"lastname": "Gnarr",
# voller Name, inklusive Zwischennamen und eventueller Adels- oder
#Doktortitel
"fullname": "Jón Gunnar Kristinsson",
# die Kategorien, die bei der Wikipedia unten dem Prominenten
#zugeordnet sind (z.B. Mann, Frau, Komiker, Mitglied des Bundestages, etc.)
"categories": [
    "Komiker",
    "Autor",
    "Schriftsteller (Reykjavík)",
    "Bürgermeister (Reykjavík)",
    "Mitglied von Björt framtíð",
    "Isländer",
    "Geboren 1967",
    "Mann",
],
# eine Stadt, die mit dem Prominenten verbunden ist, z.B.
#Geburtsstadt, derzeitiger Wohnort, etc.
"city": "Reykjavík",
# Koordinaten
"coordinates": {
"lat": 64.15,
"lon": 21.933333333333
},
# der erste Absatz mit einführenden Worten zu dem Prominenten
"teaser": 
"""
Jón Gnarr (* 2. Januar 1967 in Reykjavík als Jón Gunnar Kristinsson) ist ein
isländischer Komiker, Musiker, Schriftsteller und Politiker. Von Juni 2010 bis
zum 17. Juni 2014 war er Bürgermeister Reykjavíks, der Hauptstadt Islands.
""",
# entweder der gesamte Artikel oder nur der Lebenslaufteil aus der
#Wikipedia kopiert
"vita":
"""
Jón Gnarr (* 2. Januar 1967 in Reykjavík als Jón Gunnar Kristinsson) ist ein
isländischer Komiker, Musiker, Schriftsteller und Politiker. Von Juni 2010 bis
zum 17. Juni 2014 war er Bürgermeister Reykjavíks, der Hauptstadt Islands.

Leben

Jón Gnarr − jüngster ehelicher Sohn eines Polizeibeamten (* 1917) und einer
Arbeiterin (* 1922; † 25. Dezember 2010) − verließ im Alter von vierzehn Jahren
ohne Abschluss eine Reykjavíker Schule, die kein herkömmliches Klassensystem
und keine Noten kannte. Seit dieser Zeit nennt er sich Jón Gnarr.^[1] Er
besuchte dann für zwei Jahre ein Internat für schwer erziehbare Jugendliche.
Nach diversen Gelegenheitsjobs arbeitete er zunächst als Pfleger in einem Heim
für geistig und körperlich Behinderte. Mit neunzehn Jahren schrieb er den Roman
Miðnætursólborgin („Die Stadt der Mitternachtssonne“).

Jón ist mit der Masseurin Jóhanna Jóhannsdóttir (Jóga), Tochter eines
Seemannes, verheiratet. Das Ehepaar hat fünf Kinder: das älteste Kind wurde
1985 und das jüngste 2005 geboren.^[2] Im Jahr 2006 schrieb Jón Gnarr eine
fiktive Autobiografie mit dem Titel Indjáninn („Der Indianer“), die er 2012 mit
Sjóræninginn („Der Seeräuber“) fortsetzte.

Seine Karriere als Komiker begann Jón Gnarr Anfang der 1990er Jahre beim
öffentlich-rechtlichen Rundfunk Ríkisútvarpið mit der Radio-Sitcom Hotel
Volkswagen. Später arbeitete er für einen privaten Sender, für den er
morgendliche „Interviews“ führte, und trat in Hörfunk, Fernsehen und in
Spielfilmen auf. Neben der Sketch-Show Fóstbræður (1997–2001) wurde er vor
allem durch seine Rolle als Georg Bjarnfreðarson in den drei Serien Næturvaktin
(2007), Dagvaktin (2008) und Fangavaktin (2009) bekannt. Jón Gnarr war laut
Munzinger-Archiv Bassist der Punkrockband Nefrennsli („laufende Nasen“)^[3],
obwohl er laut Henryk M. Broder „kein Instrument spielen und Musik nicht
leiden“ kann^[4] und wirkte in mehreren Filmen, Sitcoms und Talkshows mit.
Ferner veröffentlicht er Prosa und Lyrik.

Im Jahr 2009 − während der isländischen Finanzkrise − arbeitete Jón Gnarr in
einer Werbeagentur als Creative Director. Nachdem er sich von dieser Agentur
getrennt hatte, konzentrierte er seine Arbeit auf die Politik.^[5] Bei der
Kommunalwahl in Reykjavík am 29. Mai 2010 erzielte Jóns Partei Besti flokkurinn
mit 34,7 % die meisten Stimmen.^[6] Nach dem Wahlsieg übernahm Jón am 15. Juni
2010 von der bisherigen Bürgermeisterin Hanna Birna Kristjánsdóttir das Amt des
Bürgermeisters.^[7] Auf der Liste der Partei kandidierten Musiker,
Schauspieler, Comic-Zeichner und weitere Prominente.^[8] Zum Wahlprogramm
gehörten folgende Punkte:^[9]

 1. Offene statt heimliche Korruption.
 2. Kostenlose Handtücher für alle Schwimmbäder.
 3. Ein Eisbär für Reykjavíks Zoo.

Auf nationaler Ebene ist Jón Gnarr Mitglied im Vorstand der 2012 gegründeten
Partei Björt framtíð,^[10] die in enger Verbindung mit Besti flokkurinn steht.
Im Oktober 2013 gab Jón Gnarr bekannt, dass er keine zweite Amtszeit als
Bürgermeister von Reykjavík anstreben wird.^[11] Seine Amtszeit endete am 17.
Juni 2014. Jóns Nachfolger wurde sein bisheriger Koalitionspartner Dagur B.
Eggertsson von der sozialdemokratischen Allianz (Samfylkingin). Dagur äußerte
anlässlich der Amtsübergabe, „die ganze Gesellschaft“ habe von Jón Gnarr
gelernt.^[12]

Während der Amtszeit von Jón Gnarr wurde die Online-Plattform Betri Reykjavik
(Better Reykjavik) als eine Möglichkeit direkter Demokratie etabliert.^[13]

Jón Gnarr konvertierte vom evangelischen zum römisch-katholischen Glauben,^[4]
bezeichnet sich aber als Atheisten ^[14] und Anarchisten.^[15]

Veröffentlichungen

Isländische Sprache

  • Miðnætursólborgin. Smekkleysa, Reykjavík 1989.
  • Plebbabókin. Mál og menning, Reykjavík 2002, ISBN 9979-3-2373-6.
  • Þankagangur. Skálholtsútgáfan, Reykjavík 2005, ISBN 9979-792-06-X.
  • Indjáninn. Skálduð ævisaga. Mál og menning, Reykjavík 2006, ISBN
    9979-3-2792-8.
  • Sjóræninginn. Skálduð ævisaga. Mál og menning, Reykjavík 2012, ISBN
    978-9979-3-3320-3.

Deutsche Sprache

  • Hören Sie gut zu und wiederholen Sie!!! Wie ich einmal Bürgermeister wurde
    und die Welt veränderte. Mitarbeit: Jóhann Ævar Grímsson. Aus dem
    Isländischen von Betty Wahl. Klett-Cotta, Stuttgart 2014, ISBN
    978-3-608-50322-7.

Film

  • Jón Gnarr - Mein Reykjavik. Dokumentarfilm, Österreich, 2014, 51:20 Min.,
    Buch und Regie: Günter Schilhan, Produktion: ORF, 3sat, Reihe: Meine Stadt,
    Erstsendung: 15. Dezember 2014 bei 3sat, Inhaltsangabe von 3sat.
"""
}

jon_gnarr = ('politician', jon_gnarr_data)


with open("output.jsonl", "w") as output:
        for star_type, star_data in (steve_martin, bill_callahan, jon_gnarr):
            # generate a doc_id dependent on the star's fullname
            doc_id = hashlib.sha1()
            doc_id.update(star_data["fullname"])
            # this is the index directive for the bulk API we will be using

            output.write('{"index":{"_index":"people","_type":"%s","_id":"%s"}}\n'
                % (star_type, doc_id.hexdigest()))
            output.write(json.dumps(star_data))
            output.write("\n")
