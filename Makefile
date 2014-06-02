ifndef CXX
	CXX=g++
endif

all:
	$(CXX) -std=c++11 -g cpp/cfg-test.cpp -o cfg-test
	$(CXX) -std=c++11 -g cpp/cfg-cnf-gnf-test.cpp -o cfg-catch

test: all
	./cfg-test
	./cfg-catch
