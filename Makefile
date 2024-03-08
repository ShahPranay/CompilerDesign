cc: cc.cpp c.tab.cpp c.lex.cpp
	g++ -I. c.tab.cpp c.lex.cpp cc.cpp -lm -lfl -o $@

c.tab.cpp c.tab.hpp: c.y
	bison  -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean::
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc c.output
