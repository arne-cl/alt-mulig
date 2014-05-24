
#include "cfg.hpp"
#include <iostream>
#include <string>

int main()
{
    CFGRule::Symbol l = "NP";
    CFGRule::SymbolVector r = {"Det", "N"}; // C++11 initializer list assignment
    
    CFGRule rule1 = CFGRule(l, r);
    std::cout << "rule1: " << rule1 << std::endl;

    CFGRule rule2 = CFGRule("A", {"a", "a"});
    std::cout << "rule2: " << rule2 << std::endl;
    
    if (rule2 < rule1) {std::cout << "rule2 < rule1\n\n";}

    ContextfreeGrammar cfg;
    cfg.add_rule(CFGRule("S", {"NP", "VP"}));
    cfg.add_rule(CFGRule("PP", {"P", "NP"}));
    cfg.add_rule(CFGRule("NP", {"Det", "N"}));
    cfg.add_rule(CFGRule("NP", {"NP", "PP"}));
    cfg.add_rule(CFGRule("VP", {"V", "NP"}));
    cfg.add_rule(CFGRule("VP", {"VP", "PP"}));
    cfg.add_rule(CFGRule("NP", {"NE"}));
    cfg.add_rule(CFGRule("NP", {"NE", "NE"}));
    cfg.add_rule(CFGRule("Det", {"'a'"}));
    cfg.add_rule(CFGRule("Det", {"'the'"}));
    cfg.add_rule(CFGRule("Det", {"'one'"}));
    cfg.add_rule(CFGRule("N", {"'dog'"}));
    cfg.add_rule(CFGRule("N", {"'cat'"}));
    cfg.add_rule(CFGRule("N", {"'politician'"}));
    cfg.add_rule(CFGRule("V", {"'chased'"}));
    cfg.add_rule(CFGRule("V", {"'sat'"}));
    cfg.add_rule(CFGRule("V", {"'killed'"}));
    cfg.add_rule(CFGRule("P", {"'on'"}));
    cfg.add_rule(CFGRule("P", {"'in'"}));
    cfg.add_rule(CFGRule("NE", {"'David'"}));
    cfg.add_rule(CFGRule("NE", {"'Hasselhoff'"}));
    cfg.add_rule(CFGRule("NE", {"'George'"}));
    cfg.add_rule(CFGRule("NE", {"'Micheal'"}));
    
    std::cout << "toy grammar with " << cfg.size() << " rules:" << std::endl;
    std::cout << cfg << std::endl;
    
    std::cout << "rules w/ 'NP' as lhs:" << std::endl;
    std::set<CFGRule> nt_rules = cfg.rules_for("NP");
    for (std::set<CFGRule>::const_iterator r = nt_rules.begin();
        r!= nt_rules.end(); ++r) {
            std::cout << "\t" << *r;
        }
        
    std::cout << "\n\nstart symbol before setting it: " << cfg.get_startsymbol() << std::endl;
    cfg.set_startsymbol("S");
    std::cout << "start symbol after setting it: " << cfg.get_startsymbol() << std::endl;
}
