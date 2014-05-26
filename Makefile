ifndef CXX
	CXX=g++
endif

all:
	$(CXX) -std=c++11 -g cpp/cfg-test.cpp -o cfg-test

test: all
	./cfg-test
