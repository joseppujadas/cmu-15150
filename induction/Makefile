FILES = code/naturals/recursion.sml code/pascal/pascal.sml code/heads-or-tails/heads-or-tails.sml code/flipOne/flipOne.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
