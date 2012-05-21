INSTALL_DIR=/usr/local/bin
CFLAGS=-I. -O2 -Wall

OBJS=main.o parse_args.o

all: cl

cl: $(OBJS)
	$(CC) -o cl $(OBJS)

parse_args.c: parse-args.lisp
	@echo 'char *parse_args = ' > parse_args.c
	@sed 's/\\/\\\\/g' < parse-args.lisp | sed 's/;.*//g' | sed 's/"/\\"/g' | sed 's/$$/"/g' | sed 's/^/"/g' | sed 's/%/%%/g' | sed 's/^"[ \t]*"/""/g' | grep -v '^""' >> parse_args.c
	@echo ';\n' >> parse_args.c


install: cl
	install -c cl $(INSTALL_DIR)/cl

clean:
	rm -f *.o parse_args.c cl
