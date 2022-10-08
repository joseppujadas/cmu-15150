FILES = code/multilingual/languages.sml code/routing/routing.sml code/intersect/intersect.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
