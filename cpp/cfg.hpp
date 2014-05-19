// a context free grammar class (C++11)
// compiles with: g++/clang++ -std=c++11 cfg-test.cpp
// Arne Neumann, 2014-05-19

#include <vector>
#include <set>
#include <iostream>
#include <string>

typedef std::string LHS;
typedef std::vector<std::string> RHS;
typedef std::string Nonterminal;
typedef std::string Terminal;
typedef std::string StartSymbol;

class CFGRule
{
private: // instance variables
    LHS lhs;
    RHS rhs;

public:
    CFGRule(const LHS& l, const RHS& r)
    {
        lhs = l;
        rhs = r;
    }

    inline const LHS& get_lhs() const {return lhs;}
    
    bool operator<(const CFGRule& other) const
    {
        if (get_lhs() < other.get_lhs()) {return true;}
        if (rhs < other.rhs) {return true;}
        return false;
    }
    
  /// prints CFGRule in 'A -> B C D' style
  void print(std::ostream& o) const
  {
    o << get_lhs() << " -> ";
    for (RHS::const_iterator e = rhs.begin(); e != rhs.end(); ++e) {
      o << *e << " ";
    }
    o << std::endl;
  }
}; // CFGRule


// can't define this before CFGRule is defined
typedef std::set<CFGRule> Rules;

class ContextfreeGrammar
{
private: // instance variables
    std::set<Nonterminal> nts; // so far unused
    std::set<Terminal> ts; // so far unused
    StartSymbol startsym;
    Rules rules;

public:
    inline const unsigned size() const {return rules.size();}

    inline const StartSymbol& get_startsym() const {return startsym;}
    
    inline void set_startsym(const StartSymbol& s) {startsym = s;}
    
    inline void add_rule(const CFGRule& r) {rules.insert(r);}
    
    /// returns all rules w/ nt as lhs
    const Rules rules_for(const Nonterminal& nt) const {
        Rules nt_rules;
        for (Rules::const_iterator it = rules.begin();
            it != rules.end(); ++it) {
                if (it->get_lhs() == nt) {nt_rules.insert(*it);}
        }
        return nt_rules; // can't return ref to local object
    }
    
    /// prints all CFG rules, one per line
    void print(std::ostream& o) const
    {
        for (Rules::const_iterator r = rules.begin();
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
