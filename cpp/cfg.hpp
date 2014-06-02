// a context free grammar class (C++11)
// compiles with: g++/clang++ -std=c++11 cfg-test.cpp
// Arne Neumann, 2014-05-19

#include <vector>
#include <set>
#include <map>
#include <iostream>
#include <string>
#include <iterator>


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
        if (lhs == other.lhs) {return rhs < other.rhs;}
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
    typedef CFGRule::Symbol Symbol;
    typedef CFGRule::SymbolVector SymbolVector;
    typedef SymbolVector::const_iterator SymbolVectorIter;
    typedef std::set<Symbol> SymbolSet;
    typedef SymbolSet::const_iterator SymbolSetIter;
    typedef std::set<CFGRule> RuleSet;
    typedef RuleSet::const_iterator RuleSetIter;
    typedef std::map<Symbol,RuleSet> RuleMap;
    typedef RuleMap::const_iterator RuleMapIter;

private: // instance variables
    SymbolSet nonterminals;
    SymbolSet vocabulary; // incl. terminals & nonterminals
    Symbol start;
    RuleMap productions;

public: // instance functions
    inline const unsigned size() const {return no_of_rules();}

    inline const Symbol& get_startsymbol() const {return start;}

    inline const SymbolSet& get_nonterminals() const {return nonterminals;}

    inline const SymbolSet& get_vocabulary() const {return vocabulary;}

    inline void set_startsymbol(const Symbol& s) {
        start = s;
        nonterminals.insert(s);
        vocabulary.insert(s);
    }

    inline void add_rule(const CFGRule& r) {
        const Symbol& left = r.get_lhs();
        const SymbolVector& right = r.get_rhs();
        nonterminals.insert(left);
        vocabulary.insert(left);
        vocabulary.insert(right.begin(), right.end());

        RuleMap::iterator f = productions.find(left);

        if (f == productions.end()) { // lhs not in rules, yet
            productions.insert(RuleMap::value_type(left, RuleSet({r})));
        }
        else {
            // there's already a rule w/ left as its lhs
            // so, we'll add this rule to the rule set
            f->second.insert(r);
        }
    }

  /// returns the number of rules
    unsigned no_of_rules() const {
        unsigned count = 0;
        // iterate over all nonterminals
        for (RuleMapIter nt = productions.begin(); nt != productions.end(); ++nt) {
            // add the number of expansions (valid right-hand sides for a lhs)
            count += nt->second.size();
        }
        return count;
    }

    /// true, iff all rules are in Chomsky normal form
    /// A -> B C; A -> a; S -> epsilon
    //~ bool is_in_chomsky_nf() {
        //~ for (const CFGRule& r: productions) {
            //~ const SymbolVector& right = r.get_rhs();
            //~ const unsigned rhs_len = right.size();
//~
            //~ // no more than 2 symbols on the right-hand side
            //~ if (rhs_len > 2) {return false;}
//~
            //~ // A -> B C
            //~ if (rhs_len == 2) {
                //~ for (const Symbol& s : right) {
                     //~ // rhs symbol must be a nonterminal
                     //~ if (nonterminals.find(s) == nonterminals.end()) {return false;}
                     //~ // rhs symbol can't be the start symbol
                     //~ if (s == get_startsymbol()) {return false;}
                //~ }
                //~ continue; // this rule is in CNF
            //~ }
//~
            //~ // A -> a
            //~ if (rhs_len == 1) {
                //~ // rhs must consist of one (and only one terminal)
                //~ if (nonterminals.find(right[0]) != nonterminals.end()) {return false;}
                //~ continue; // this rule is in CNF
            //~ }
//~
            //~ // S(tart) -> epsilon
            //~ const Symbol& left = r.get_lhs();
            //~ if (rhs_len == 0 && left != get_startsymbol()) {return false;}
        //~ }
        //~ return true;
    //~ }

    /// true, iff all rules are in Chomsky normal form
    /// A -> a A_1 A_2 ... A_n; S -> epsilon
    //~ bool is_in_greibach_nf() {
        //~ for (const CFGRule& r: productions) {
            //~ const SymbolVector& right = r.get_rhs();
            //~ const unsigned rhs_len = right.size();
//~
            //~ /// A -> a A_1 A_2 ... A_n
            //~ // first symbol of rhs must be a terminal
            //~ if (rhs_len >= 1) {
                //~ if (nonterminals.find(right[0]) != nonterminals.end()) {return false;}
                //~ if (rhs_len > 1) {
                    //~ // start loop over rhs with 2nd symbol
                    //~ SymbolVectorIter r = std::next(right.begin());
                    //~ for (; r != right.end(); ++r) {
                        //~ // A_1 A_2 ... A_n must be nonterminals
                        //~ if (nonterminals.find(*r) == nonterminals.end()) {return false;}
                        //~ // A_1 A_2 ... A_n must include the start symbol
                        //~ if (*r == get_startsymbol()) {return false;}
                    //~ }
                    //~ continue; // this rule (rhs_len > 1) is in GNB
                //~ }
                //~ continue; // this rule (rhs_len == 1) is in GNB
            //~ }
//~
            //~ /// S(tart) -> epsilon
            //~ const Symbol& left = r.get_lhs();
            //~ if (rhs_len == 0 && left != get_startsymbol()) {return false;}
        //~ }
        //~ return true;
    //~ }


    /// returns all rules w/ nt as lhs
    const RuleSet& rules_for(const Symbol& nonterminal) const {
        RuleMapIter f = productions.find(nonterminal);
        static const RuleSet empty_ruleset;
        if (f == productions.end()) {return empty_ruleset;}
        else {
            // f points to a std::pair<Symbol,RuleSet>
            return f->second;
        }
    }

    /// prints all CFG rules, one per line
    void print(std::ostream& o) const {
        for (RuleMapIter nt = productions.begin(); nt != productions.end(); ++nt) {
            const RuleSet& rules = nt->second;
            for (RuleSetIter r = rules.begin(); r != rules.end(); ++r) {
                r->print(o);
            }
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
