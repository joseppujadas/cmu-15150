FILES = code/diff/diff.sml code/rpn/rpn.sml code/intervals/intervals.sml code/rose/rose.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
