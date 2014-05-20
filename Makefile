all:
	g++ -std=c++11 cpp/cfg-test.cpp -o cfg-test

test: all
	./cfg-test
