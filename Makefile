CC = g++
CFLAGS = -g -O0 -std=c++11

SRC  = lib.cpp                        # list of C++ source files
OBJS = $(patsubst %.cpp, %.o, $(SRC)) # list of object files


miniL: miniL-lex.o miniL-parser.o $(OBJS)
	$(CC) $^ -o $@ -lfl

%.o: %.cpp
	$(CC) $(CFLAGS) -c $< -o $@

miniL-lex.cpp: miniL.lex miniL-parser.cpp
	flex -o $@ $< 

miniL-parser.cpp: miniL.y
	bison -d -v -g -o $@ $<

clean:
	rm -f *.o miniL-lex.cpp miniL-parser.cpp miniL-parser.hpp stack.hh *.output *.dot miniL