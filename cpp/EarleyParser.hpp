#ifndef __EARLEYPARSER_HPP__
#define __EARLEYPARSER_HPP__

#include "ContextFreeGrammar.hpp"

class EarleyParser
{
public:
  EarleyParser(const ContextFreeGrammar& g, const Lexicon& l) 
  : cfg(g), lexicon(l)
  {
  }
  
  /// Tokenisiert und 
  bool parse(const std::string& s) 
  {
  }  
  
private:
  const ContextFreeGrammar& cfg;
}; // EarleyParser

#endif
