
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <string>
#include <iostream>

#include <boost/algorithm/string.hpp>

class Lexicon
{
public: // type definitions
  typedef std::unordered_set<std::string> POSSet;
  typedef std::unordered_map<std::string, POSSet> LexiconMap;


public: // functions, constructors
  /// construct from input stream (file or cin)
  Lexicon(std::istream& lexicon_in)
  {
    read_in(lexicon_in);
  }

    /// returns True, iff the given word is in the lexicon
    bool contains_word(const std::string& word)
    {
        return lexicon.find(word) != lexicon.end();
    }

    /// returns True, iff the given POS tag is one of the POS tags attributed
    /// to the given word in the lexicon
    bool has_postag(const std::string& word, const std::string& pos)
    {
        if (contains_word(word))
        {
            const POSSet postags = get_postags(word);
            return postags.find(pos) != postags.end();
        }
        else {return false;}
    }

    /// returns the set of all possible POS tags for the given word
    const POSSet get_postags(const std::string& word)
    {
        static const POSSet empty_posset;
        return (contains_word(word)) ? lexicon[word] : empty_posset;
    }

private:  // functions
  /// reads a lexicon (from file, cin ...)
  /// returns true, iff reading the file didn't fail
  bool read_in(std::istream& lexicon_in)
  {
    std::string line;
    unsigned line_no = 1;

    // read lexicon entries from file
    while (lexicon_in.good()) {
      std::getline(lexicon_in, line);
      if (!line.empty()) {
        std::vector<std::string> lex_entry;
        boost::split(lex_entry, line, boost::is_any_of("\t"));
        if (lex_entry.size() == 2) {
            const std::string& word = lex_entry[0];
            const std::string& pos = lex_entry[1];
            lexicon[word].insert(pos);
        }
        else {
            std::cerr << "Lexicon: Warning (line " << line_no << "): entry ignored\n";
        }
      }
      ++line_no;
    } // while

    return true;
  }

private: // instance variables
    LexiconMap lexicon;
}; // Lexicon
