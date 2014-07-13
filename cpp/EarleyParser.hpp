#ifndef __EARLEYPARSER_HPP__
#define __EARLEYPARSER_HPP__

#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <string>
#include <iterator>
#include <algorithm>
#include <deque>
#include <cassert>

#include <boost/tokenizer.hpp>

#include "ContextFreeGrammar.hpp"
#include "Lexicon.hpp"
#include "ParseForest.hpp"

class EarleyParser
{
public:
  typedef ContextFreeGrammar::Symbol  Symbol;
  typedef std::vector<std::string>    StringVector;
  
public:
  EarleyParser(const ContextFreeGrammar& g, const Lexicon& l) 
  : cfg(g), lexicon(l), PassiveSymbol("."), SuperStartSymbol("$START$") 
  {
    start_rule.set(SuperStartSymbol,CFGRule::SymbolVector(1,cfg.get_start_symbol()));
    if (cfg.has_epsilon_rules()) {
      std::cerr << "earley-parser: warning: epsilon rules are currently not supported and will be ignored\n";
    }
  }

  bool parse(const StringVector& input) 
  {
    reset();
    chart.resize(input.size()+1);
    parse_input(input);
    return success(input);
  }

  const ParseForest& get_parses() const
  {
    static const ParseForest no_parses;
    return no_parses;
  }
  
  void reset()
  {
    chart.clear();
    lexicon_rules.clear();
    parse_forest.clear();
  }
  
  void print_chart(std::ostream& o) const
  {
    // Iterate over the chart
    for (unsigned pos = 0; pos < chart.size(); ++pos) {
      std::cout << "---------------------------------------------------" << std::endl;
      std::cout << "Position: " << pos << std::endl;
      std::cout << "---------------------------------------------------" << std::endl;
      const EarleyItemMap& chart_at_pos = chart[pos];
      for (EarleyItemMap::const_iterator e = chart_at_pos.begin(); e != chart_at_pos.end(); ++e) {
        for (EarleyItemSet::const_iterator i = e->second.begin(); i != e->second.end(); ++i) {
          o << EarleyItem(i->get_dotted_rule(),i->get_back_ptr(),pos) << std::endl;
        }
      }
    }
    o << std::endl;
  }
  
private: // Types
  typedef unsigned short                    DotPosition;
  typedef unsigned short                    Position;

  /// DottedCFGRule represents a dotted CFG rule
  class DottedCFGRule 
  {
  public:
    DottedCFGRule(const CFGRule& r, DotPosition dp = 0)
    : rule(r), dot_pos(dp) {}
        
    /// Returns a new dotted rule instance where the dot is advanced
    DottedCFGRule advance() const
    {
      assert(!is_passive());
      return DottedCFGRule(rule,dot_pos+1);
    }

    bool is_passive() const
    {
      return dot_pos == rule.arity();
    }
    
    const Symbol& get_symbol_after_dot() const
    {
      static const Symbol no_symbol;
      return is_passive() ? no_symbol : rule[dot_pos];
    }
    
    friend bool operator<(const DottedCFGRule& r1, const DottedCFGRule& r2)
    {
      return (r1.rule < r2.rule) || (!(r2.rule < r1.rule) && r1.dot_pos < r2.dot_pos);
    }
    
    friend bool operator==(const DottedCFGRule& r1, const DottedCFGRule& r2)
    {
      return r1.rule == r2.rule && r1.dot_pos == r2.dot_pos;
    }

    friend std::ostream& operator<<(std::ostream& o, const DottedCFGRule& dotted_rule)
    {
      o << dotted_rule.get_lhs() << " --> ";
      std::copy(dotted_rule.rule.get_rhs().begin(), 
                dotted_rule.rule.get_rhs().begin()+dotted_rule.get_dot_pos(),
                std::ostream_iterator<CFGRule::Symbol>(std::cout, " "));
      o << " * ";
      std::copy(dotted_rule.rule.get_rhs().begin()+dotted_rule.get_dot_pos(), 
                dotted_rule.rule.get_rhs().end(),
                std::ostream_iterator<CFGRule::Symbol>(std::cout, " "));
      return o;
    }

    const CFGRule& get_rule() const { return rule; }
    const Symbol& get_lhs()   const { return rule.get_lhs(); }
    DotPosition get_dot_pos() const { return dot_pos; }

  private:
    const CFGRule& rule;
    DotPosition dot_pos;  
  }; // DottedCFGRule

  class DottedCFGRuleWithBackPtr
  {
  public:
    DottedCFGRuleWithBackPtr(const DottedCFGRule& dr, Position bp)
    : dotted_rule(dr), back_ptr(bp) {}
    
    bool operator<(const DottedCFGRuleWithBackPtr& drbp) const
    {
      return (dotted_rule < drbp.dotted_rule) ||
             (!(drbp.dotted_rule < dotted_rule) && back_ptr < drbp.back_ptr);
    }
    
    const DottedCFGRule& get_dotted_rule()  const { return dotted_rule; }
    Position get_back_ptr()                 const { return back_ptr; }
    const Symbol& get_lhs()                 const { return dotted_rule.get_lhs(); }
    
  private:
    DottedCFGRule dotted_rule;
    Position back_ptr;
  }; // DottedCFGRuleWithBackPtr
  
  
  /// Earley-Item repräsentiert ein Chart-Item des Earley-Parsers
  class EarleyItem
  {
  public:
    EarleyItem(const DottedCFGRule& dr, Position bp, Position r)
    : dotted_rule_with_bp(dr,bp), right_boundary(r) {}
    
    bool is_active() const
    {
      return !dotted_rule_with_bp.get_dotted_rule().is_passive();
    }

    const Symbol& get_symbol_after_dot() const
    {
      return dotted_rule_with_bp.get_dotted_rule().get_symbol_after_dot();
    }
    
    const Symbol& get_lhs() const
    {
      return dotted_rule_with_bp.get_dotted_rule().get_lhs();
    }

    Position get_back_ptr() const { return dotted_rule_with_bp.get_back_ptr(); }
    Position get_right_boundary() const { return right_boundary; }
      
    const DottedCFGRuleWithBackPtr& get_dotted_rule_with_bp() const
    {
      return dotted_rule_with_bp;
    }
    
    const DottedCFGRule& get_dotted_rule() const
    {
      return dotted_rule_with_bp.get_dotted_rule();
    }

    friend std::ostream& operator<<(std::ostream& o, const EarleyItem& item)
    {
      return o << "(" << item.get_back_ptr() << "," << item.get_dotted_rule() << "," 
               << item.right_boundary << ")";
    }

  private:
    DottedCFGRuleWithBackPtr dotted_rule_with_bp;
    Position right_boundary;
  }; // EarleyItem
  
  typedef std::vector<std::string>            TokenVector;
  typedef std::set<DottedCFGRuleWithBackPtr>  EarleyItemSet;
  typedef std::map<Symbol,EarleyItemSet>      EarleyItemMap;
  typedef std::vector<EarleyItemMap>          Chart;
  typedef std::deque<EarleyItem>              EarleyItemQueue;
  typedef Lexicon::LexInfoVector              LexInfoVector;
  typedef Lexicon::LexInfo                    LexInfo;
  typedef ContextFreeGrammar::LHSRange        RuleRange;

private:
  void parse_input(const StringVector& input)
  {
    enqueue(EarleyItem(DottedCFGRule(start_rule),0,0));
    while (!item_queue.empty()) {
      EarleyItem item = item_queue.front();
      item_queue.pop_front();
      if (item.is_active()) {
        if (cfg.is_nonterminal(item.get_symbol_after_dot())) {
          predict(item);
        }
        else if (cfg.is_preterminal(item.get_symbol_after_dot())) {
          // If we use a separate lexicon we do a scan based on the category of the input word
          preterminal_scan(item,input);
        }
        else {
          // No lexicon => scan input word
          lexical_scan(item,input);
        }  
      }
      else {
        complete(item);
      }
    } // while
    
    print_chart(std::cout);
  }
  
  bool success(const StringVector& input) const
  {
    return in_chart(EarleyItem(DottedCFGRule(start_rule,1),0,input.size()));
  }
  
  void predict(const EarleyItem& item)
  {
  }
  
  void preterminal_scan(const EarleyItem& item, const StringVector& input)
  {
  }

  void lexical_scan(const EarleyItem& item, const StringVector& input)
  {
  }
  
  void complete(const EarleyItem& item)
  {
  }

  bool in_chart(const EarleyItem& item) const 
  {
    // We assume that the item has the form (i, A -> alpha * B beta, j)
    assert(item.get_back_ptr() < chart.size());
    // First look into the appropriate chart cell j
    const EarleyItemMap& chart_i = chart[item.get_right_boundary()];
    // Then check whether the item is active
    EarleyItemMap::const_iterator f = item.is_active() 
      ? chart_i.find(item.get_symbol_after_dot()) 
      : chart_i.find(PassiveSymbol);
    
    if (f != chart_i.end()) {
      // There are items with B after the dot
      EarleyItemSet::const_iterator f2 = f->second.find(item.get_dotted_rule_with_bp());
      return f2 != f->second.end();
    }
    else return false;
  }

  bool enqueue(const EarleyItem& item)
  {
    if (!in_chart(item)) {
      item_queue.push_back(item);
      insert_chart(item);
      return true;
    }
    else return false;
  }
  
  /// Insert item into the chart
  void insert_chart(const EarleyItem& item)
  {
    // First look into the appropriate chart cell j
    EarleyItemMap& chart_i = chart[item.get_right_boundary()];
    // Determine key
    const Symbol& key = (item.is_active() ? item.get_symbol_after_dot() : PassiveSymbol);
    chart_i[key].insert(item.get_dotted_rule_with_bp());
  }

private: // Instanzvariablen
  const ContextFreeGrammar& cfg;              ///< Grammar
  const Lexicon&            lexicon;          ///< Lexicon
  Chart                     chart;            ///< The parse chart
  EarleyItemQueue           item_queue;       ///< Agenda
  ParseForest               parse_forest;     ///< Parse forest
  const Symbol              PassiveSymbol;    ///< Symbol used as key for passive items
  const Symbol              SuperStartSymbol; ///< Super start symbol of the grammar
  CFGRule                   start_rule;       ///< Super --> S
}; // EarleyParser

#endif
