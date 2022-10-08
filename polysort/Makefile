FILES = code/lca/lca.sml code/poly-quicksort/quicksort.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
