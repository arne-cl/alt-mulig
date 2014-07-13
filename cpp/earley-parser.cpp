////////////////////////////////////////////////////////////////////////////////
// earley-parser.cpp
// An Earley-parser in C++
// TH, 10.7.14
// Try: earley-parser gram1.grm demo.lex "die Katze kennt die Maus"
////////////////////////////////////////////////////////////////////////////////


#include <string>
#include <fstream>
#include <iostream>
#include <iterator>
#include <boost/tokenizer.hpp>

#include "EarleyParser.hpp"
#include "Lexicon.hpp"


int main(int argc, char* argv[])
{
  const std::string usage = "earley-parser GRAMMARFILE LEXICONFILE \"INPUT\"";
  // Set to true if we use a lexicon instead of a lexical rules in the grammar
  const bool use_separate_lexicon = true; 
  
  if (argc != 4) {
    std::cerr << "earley-parser: bad number of arguments\n";
    std::cerr << "Usage: " << usage << std::endl;
    exit(1);
  }

  std::ifstream grm_in(argv[1]);
  if (!grm_in) {
    std::cerr << "Unable to open grammar file '" << argv[1] << "'.\n";
    std::cerr << "Usage: " << usage << std::endl;
    exit(1);
  }

  std::ifstream lex_in(argv[2]);
  if (!grm_in) {
    std::cerr << "Unable to open lexicon file '" << argv[1] << "'.\n";
    std::cerr << "Usage: " << usage << std::endl;
    exit(1);
  }

  // Tokenize input
  std::string input_str(argv[3]);
  boost::tokenizer<boost::char_separator<char> > tokens(input_str,boost::char_separator<char>(" "));
  EarleyParser::StringVector input(tokens.begin(), tokens.end()); 
  
  // Construct lexicon and grammar objects
  Lexicon lexicon(lex_in);
  ContextFreeGrammar grammar(grm_in,use_separate_lexicon);
  
  EarleyParser parser(grammar,lexicon);
  if (parser.parse(input)) {
    std::cerr << "earley-parser: parsing successful" << std::endl;
  }
  else {
    std::cerr << "earley-parser: no parse found for input" << std::endl;
  }
}
