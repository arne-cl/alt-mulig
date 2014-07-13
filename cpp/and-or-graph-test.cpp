

#include <fstream>
#include <map>
#include <cassert>

#include "ContextFreeGrammar.hpp"
#include "AndOrGraph.hpp"

int main(int argc, char* argv[])
{
  typedef ContextFreeGrammar::Symbol      NodeType;
  typedef AndOrGraph<NodeType>            CFGAndOrGraph;
  typedef CFGAndOrGraph::AndNodes         AndNodes;
  typedef CFGAndOrGraph::OrNodes          OrNodes;
  typedef ContextFreeGrammar::SymbolSet   SymbolSet;
  
  if (argc != 2) {
    std::cerr << "Missing grammar file argument.\n";
    std::cerr << "Usage: and-or-graph-test GRMFILE\n";
    exit(1);
  }

  std::ifstream grm_in(argv[1]);
  if (!grm_in) {
    std::cerr << "Unable to open '" << argv[1] << "'.\n";
    std::cerr << "Usage: and-or-graph-test GRMFILE\n";
    exit(1);
  }

  ContextFreeGrammar cfg(grm_in);
  CFGAndOrGraph grm_and_or_graph;
  
  // Regeln der CFG in den Und-Oder-Graphen einfügen
  for (ContextFreeGrammar::const_iterator r = cfg.begin(); r != cfg.end(); ++r) {
    grm_and_or_graph.add_arc(r->get_lhs(),AndNodes(r->get_rhs().begin(),r->get_rhs().end()));
  }

  ContextFreeGrammar::SymbolSet sigma = cfg.alphabet();
  for (SymbolSet::const_iterator s = sigma.begin(); s != sigma.end(); ++s) {
    grm_and_or_graph.add_node(*s);
  }
  
  // Und-Oder-Graphen visualisieren
  grm_and_or_graph.draw(std::ofstream("cfg.dot"));
}
