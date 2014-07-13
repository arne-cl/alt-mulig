#ifndef __LEXICON_HPP__
#define __LEXICON_HPP__

#include <vector>
#include <string>
#include <iostream>

#include <boost/unordered_map.hpp>
#include <boost/tokenizer.hpp>

/// Lexicon implementiert ...
class Lexicon
{
public:
  typedef std::string Word;
  typedef std::string Lemma;
  typedef std::string Tag;
  typedef std::string Morph;

  /// LexInfo kodiert einen einzelnen Lexikoneintrag, bestehend aus
  /// POS-Tag, Lemma und morph. Analyse
  struct LexInfo
  {
    LexInfo(const Tag& t, const Word& l, const Morph& m) 
    : tag(t), lemma(l), morph(m) {}
    
    const Tag& get_tag() const { return tag; }
    const Word& get_lemma() const { return lemma; }
    const Morph& get_morph() const { return morph; }
    float get_log_prob() const { return log_prob; }
    void set_log_prob(float p) { log_prob=p; }
       
    Tag tag;
    Word lemma;
    Morph morph;
    float log_prob;
  }; // LexInfo
  
  typedef std::vector<LexInfo> LexInfoVector;
  
public:
  /// Konstruktor
  Lexicon(std::istream& in) : num_lex_entries(0)
  {
    read_in(in);
  }
  
  /// Fügt ein Wort zusammen mit POS-Tag, Lemma und Morph-Information in das Lexikon ein.
  void add_word(const Word& w, const Tag& t, const Word& l, const Morph& m)
  {
    word_map[w].push_back(LexInfo(t,l,m));
    ++num_lex_entries;
  }
  
  /// Schlägt w im Lexicon nach und gibt einen Vektor mit den Analysen zurück
  const LexInfoVector& word_info(const Word& w) const
  {
    static const LexInfoVector no_info;
    LexiconMap::const_iterator fw = word_map.find(w);
    return (fw != word_map.end()) ? fw->second : no_info;
  }
  
  /// Gibt die Anzahl der momentan im Lexikon vorhandenen Einträge zurück
  unsigned lex_entry_count() const 
  { 
    return num_lex_entries; 
  }
  
  /// Gibt die Anzahl der momentan im Lexikon vorhandenen Wörter zurück
  unsigned word_count() const 
  { 
    return word_map.size(); 
  }

private:
  /// Definition der internen Lexikondatenstruktur
  typedef boost::unordered_map<Word,LexInfoVector>  LexiconMap;
  
private:
  void read_in(std::istream& in)
  {
    typedef boost::char_separator<char>     CharSeparator;
    typedef boost::tokenizer<CharSeparator> Tokenizer;
    
    CharSeparator tab("\t");
    std::string line;
    std::vector<std::string> vtokens;
    unsigned line_no = 1;
    while (in.good()) {
      std::getline(in,line);
      if (!line.empty()) {
        Tokenizer tokens(line,tab);
        vtokens.assign(tokens.begin(),tokens.end());
        if (vtokens.size() == 4) {
          add_word(vtokens[0],vtokens[1],vtokens[2],vtokens[3]);
        }
        else {
          std::cerr << "Lexicon: Invalid lexicon entry at line " << line_no << "\n";
        }
      }
      ++line_no;
    } // while
  }

private:
  LexiconMap word_map;
  unsigned num_lex_entries;
}; // Lexicon

#endif
