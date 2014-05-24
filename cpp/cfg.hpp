// a context free grammar class (C++11)
// compiles with: g++/clang++ -std=c++11 cfg-test.cpp
// Arne Neumann, 2014-05-19

#include <vector>
#include <set>
#include <iostream>
#include <string>


class CFGRule
{
public: // type definitions
    typedef std::string Symbol;
    typedef std::vector<Symbol> SymbolVector;

private: // instance variables
    Symbol lhs;
    SymbolVector rhs;

public:
    CFGRule(const Symbol& l, const SymbolVector& r)
    {
        lhs = l;
        rhs = r;
    }

    inline const Symbol& get_lhs() const {return lhs;}

    inline const SymbolVector& get_rhs() const {return rhs;}

    bool operator<(const CFGRule& other) const
    {
        if (lhs < other.lhs) {return true;}
        if (rhs < other.rhs) {return true;}
        return false;
    }

    // read access to rhs symbols
    const Symbol& operator[](unsigned i) const {
        static const Symbol invalid;
        return (i < rhs.size()) ? rhs[i] : invalid;
    }

  /// prints CFGRule in 'A -> B C D' style
  void print(std::ostream& o) const
  {
    o << lhs << " -> ";
    for (SymbolVector::const_iterator e = rhs.begin(); e != rhs.end(); ++e) {
      o << *e << " ";
    }
    o << std::endl;
  }
}; // CFGRule


class ContextfreeGrammar
{
public: // type definitions
    typedef std::set<CFGRule> RuleSet;
    typedef CFGRule::Symbol Symbol;
    typedef std::set<Symbol> SymbolSet;

private: // instance variables
    SymbolSet nonterminals; // so far unused
    SymbolSet terminals; // so far unused
    Symbol start;
    RuleSet rules;

public: // instance functions
    inline const unsigned size() const {return rules.size();}

    inline const Symbol& get_startsymbol() const {return start;}
    
    inline void set_startsymbol(const Symbol& s) {start = s;}
    
    inline void add_rule(const CFGRule& r) {rules.insert(r);}
    
    /// returns all rules w/ nt as lhs
    const RuleSet rules_for(const Symbol& nonterminal) const {
        RuleSet nt_rules;
        for (RuleSet::const_iterator it = rules.begin();
            it != rules.end(); ++it) {
                if (it->get_lhs() == nonterminal) {nt_rules.insert(*it);}
        }
        return nt_rules; // can't return ref to local object
    }

    /// prints all CFG rules, one per line
    void print(std::ostream& o) const
    {
        for (RuleSet::const_iterator r = rules.begin();
            r != rules.end(); ++r) {
            r->print(o);
        }
    }
}; // ContextfreeGrammar

/// overloaded output operator for CFGRule
std::ostream& operator<<(std::ostream& o, const CFGRule& r)
{
  r.print(o);
  return o;
}

/// overloaded output operator for ContextfreeGrammar
std::ostream& operator<<(std::ostream& o, const ContextfreeGrammar& cfg)
{
  cfg.print(o);
  return o;
}
