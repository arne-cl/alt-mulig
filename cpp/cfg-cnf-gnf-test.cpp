#define CATCH_CONFIG_MAIN  // tells Catch to provide a main()

#include "catch.hpp"
#include "cfg.hpp"
#include <iostream>

TEST_CASE("Is grammar in CNF?", "[CNF]") {
    ContextfreeGrammar cfg1;
    cfg1.add_rule(CFGRule("S", {"NP", "VP"}));
    // no start symbol
    REQUIRE(cfg1.is_in_chomsky_nf() == false);
    
    cfg1.set_startsymbol("S");
    // no rules w/ nonterminals NP/VP on lhs, yet
    REQUIRE(cfg1.is_in_chomsky_nf() == false);
    
    cfg1.add_rule(CFGRule("NP", {"Horst"}));
    cfg1.add_rule(CFGRule("VP", {"died"}));
    REQUIRE(cfg1.is_in_chomsky_nf() == true);
    
    cfg1.add_rule(CFGRule("NP", {"Det", "N"}));
    // no rules w/ nonterminals Det/N on lhs, yet
    REQUIRE(cfg1.is_in_chomsky_nf() == false);
    
    cfg1.add_rule(CFGRule("Det", {"the"}));
    cfg1.add_rule(CFGRule("N", {"man"}));
    REQUIRE(cfg1.is_in_chomsky_nf() == true);
    
    SECTION("illegal epsilon rule") {
        cfg1.add_rule(CFGRule("N", {}));
        // N is not a start symbol, so N -> epsilon is not allowed
        REQUIRE(cfg1.is_in_chomsky_nf() == false);
    }
    
    SECTION("illegal NT -> NT rule") {
        cfg1.add_rule(CFGRule("NP", {"N"}));
        // NT cannot expand into another NT
        REQUIRE(cfg1.is_in_chomsky_nf() == false);
    }

    SECTION("rhs contains more than 2 symbols") {
        cfg1.add_rule(CFGRule("NP", {"the", "old", "man"}));
        REQUIRE(cfg1.is_in_chomsky_nf() == false);
    }
}


TEST_CASE("Is grammar in GNF?", "[GNF]") {
    ContextfreeGrammar cfg2;
    cfg2.add_rule(CFGRule("S", {}));
    // S isn't defined as start symbol, yet
    REQUIRE(cfg2.is_in_greibach_nf() == false);
    
    cfg2.set_startsymbol("S");
    REQUIRE(cfg2.is_in_greibach_nf() == true);
    
    cfg2.add_rule(CFGRule("NP", {"Godzilla"}));
    REQUIRE(cfg2.is_in_greibach_nf() == true);
    
    cfg2.add_rule(CFGRule("NP", {"the", "N"}));
    // no rule w/ N on lhs, yet
    REQUIRE(cfg2.is_in_greibach_nf() == false);
    
    cfg2.add_rule(CFGRule("N", {"man"}));
    REQUIRE(cfg2.is_in_greibach_nf() == true);

    SECTION("rhs begins w/ nt") {
        cfg2.add_rule(CFGRule("NP", {"Det", "N"}));
        cfg2.add_rule(CFGRule("Det", {"the"}));
        REQUIRE(cfg2.is_in_greibach_nf() == false);        
    }
    
    SECTION("rhs includes start symbol") {
        cfg2.add_rule(CFGRule("NP", {"the", "S"}));
        REQUIRE(cfg2.is_in_greibach_nf() == false);        
    }
}
