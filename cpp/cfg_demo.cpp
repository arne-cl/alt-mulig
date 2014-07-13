////////////////////////////////////////////////////////////////////////////////
// cfg_demo.cpp
// Demonstriert die Klasse für kf. Grammatiken
// TH, 3.6.14
// Aufruf: cfg_demo GRMFILE
////////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>

#include "ContextFreeGrammar.hpp"

int main(int argc, char* argv[])
{
  if (argc != 2) {
    std::cerr << "Missing grammar file argument.\n";
    std::cerr << "Usage: cfg_demo GRMFILE\n";
    exit(1);
  }

  std::ifstream grm_in(argv[1]);
  if (!grm_in) {
    std::cerr << "Unable to open '" << argv[1] << "'.\n";
    std::cerr << "Usage: cfg_demo GRMFILE\n";
    exit(1);
  }

  // Erzeuge eine Grammatikinstanz und konstruiere sie auf der Basis der Eingabedatei
  ContextFreeGrammar cfg(grm_in);
  ContextFreeGrammar::LHSRange np_rules = cfg.rules_for("NP");
  std::cout << "Die NP-Regeln sind:\n";
  for (ContextFreeGrammar::const_iterator r = np_rules.first; r != np_rules.second; ++r) {
    std::cout << *r << " ; hash(r)=" << r->hash() << "\n";
  }

  std::cout << "\nDemo des neuen Grammatik-Regeliterators:\n";
  for (ContextFreeGrammar::const_iterator r = cfg.begin(); r != cfg.end(); ++r) {
    std::cout << *r << "\n";
  }

  std::cout << "\nIst G in CNF? " << (cfg.is_in_cnf() ? "ja" : "nein") << "\n";
  std::cout << "Ist G in GNF? " << (cfg.is_in_gnf() ? "ja" : "nein") << "\n";
  std::cout << "Hat G Kettenregeln? " << (cfg.has_chain_rules() ? "ja" : "nein") << "\n";
  std::cout << "Hat G Tilgungsregeln? " << (cfg.has_epsilon_rules() ? "ja" : "nein") << "\n";
}

