# Starter Makefile written by Liam Wirth
# written to be a general purpose Makefile for C projects, in the following structure:
# root | 
# 
CC=gcc
CFLAGS=-Iinclude -Wall -Wextra -O2
SRCDIR=src
OBJDIR=obj
OUTPUT=AdventOfCode

SOURCES=$(wildcard $(SRCDIR)/*.c)
OBJECTS=$(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

.PHONY: all clean

all: $(OUTPUT)

$(OUTPUT): $(OBJECTS)
	$(CC) -o $@ $(OBJECTS)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJDIR)/*.o $(OUTPUT)
run:
	./$(OUTPUT)
