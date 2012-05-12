INSTALL_DIR=/usr/local/bin
CFLAGS=-I. -O2 -Wall

OBJS=main.o

all: cl

cl: $(OBJS)
	$(CC) -o cl $(OBJS)

install: cl
	install -c cl $(INSTALL_DIR)/cl

clean:
	rm -f *.o cl
