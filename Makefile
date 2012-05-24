INSTALL_DIR=/usr/local/bin
CFLAGS=-I. -O2 -Wall

OBJS=main.o parse_args.o shebang.o lisp_args.o create_build_file.o

all: cl

cl: $(OBJS)
	$(CC) -o cl $(OBJS)

parse_args.c: parse_args.lisp
	./lisp2cvar.sh parse_args

shebang.c: shebang.lisp
	./lisp2cvar.sh shebang

lisp_args.c: lisp_args.lisp
	./lisp2cvar.sh lisp_args

create_build_file.c: create_build_file.lisp
	./lisp2cvar.sh create_build_file

install: cl
	install -c cl $(INSTALL_DIR)/cl

clean:
	rm -f *.o parse_args.c shebang.c lisp_args.c create_build_file.c cl
