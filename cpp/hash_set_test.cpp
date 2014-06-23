// Hash-Set-Demo

#include <string>
#include <unordered_set>
#include <iostream>

typedef std::unordered_set<std::string> StringSet;
  
int main()
{
  StringSet s;
  s.insert("Haus");
  s.insert("Garten");
  s.insert("Rasen");
  s.insert("Blume");
  
  StringSet::const_iterator f = s.find("Garten");
  if (f != s.end()) {
    std::cout << *f << " ist im Lexikon drin\n";
  }
  else {
    std::cout << *f << " ist nicht im Lexikon drin\n";
  }
}
