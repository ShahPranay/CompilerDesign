cc: cc.cpp c.tab.cpp c.lex.cpp AstCodegen.o
	g++ -O3 -I. 'llvm-config --cxxflags --ldflags --system-libs --libs core' c.tab.cpp c.lex.cpp cc.cpp AstCodegen.o -lm -lfl -o $@

AstCodegen.o: AstCodegen.cpp
	g++ -O3 -I. 'llvm-config --cxxflags --ldflags --system-libs --libs core' AstCodegen.cpp -c -o AstCodegen.o

c.tab.cpp c.tab.hpp: c.y
	bison  -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean::
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc c.output AstCodegen.o
