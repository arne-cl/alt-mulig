////////////////////////////////////////////////////////////////////////////////
// lex_demo.cpp
// Demonstriert die Lexikon-Klasse
// TH, 22.6.14
// Aufruf: lex_demo LEXFILE
////////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>

#include "Lexicon.hpp"

int main(int argc, char* argv[])
{
  if (argc != 2) {
    std::cerr << "Missing grammar file argument.\n";
    std::cerr << "Usage: lex_demo LEXFILE\n";
    exit(1);
  }

  std::ifstream lex_in(argv[1]);
  if (!lex_in) {
    std::cerr << "Unable to open '" << argv[1] << "'.\n";
    std::cerr << "Usage: lex_demo LEXFILE\n";
    exit(1);
  }
  
  Lexicon lexicon(lex_in);
  std::cerr << "There are " << lexicon.word_count() << " words and " 
            << lexicon.lex_entry_count()  << " entries in " 
            << argv[1] << std::endl;

  while (true) {
    std::cout << "Enter a word: ";
    Lexicon::Word word;
    std::getline(std::cin,word);
    if (word.empty()) break;
    const Lexicon::LexInfoVector& lexinfo = lexicon.word_info(word);
    std::cout << "Analyses for " << word << std::endl;
    for (unsigned i = 0; i < lexinfo.size(); ++i) {
      std::cout << "pos=" << lexinfo[i].get_tag() << "\t"
                << "lemma=" << lexinfo[i].get_lemma() << "\t"
                << "morph=" << lexinfo[i].get_morph() << std::endl;
    }
  }
}
