
#include <iostream>
#include <fstream>
#include <string>

#include "Lexicon.hpp"


int main(int argc, char* argv[])
{
  if (argc != 2) {
    std::cerr << "Missing lexicon file argument.\n";
    std::cerr << "Usage: lexicon_demo LEXICON_FILE\n";
    exit(1);
  }

  std::ifstream lex_in(argv[1]);
  if (!lex_in) {
    std::cerr << "Unable to open '" << argv[1] << "'.\n";
    std::cerr << "Usage: lexicon_demo LEXICON_FILE\n";
    exit(1);
  }

  // Erzeuge eine Grammatikinstanz und konstruiere sie auf der Basis der Eingabedatei
  Lexicon lex(lex_in);
}
