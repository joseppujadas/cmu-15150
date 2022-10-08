FILES = code/seqfns/SeqFns.sml code/aiv-tracker/utils/Utils.sml code/aiv-tracker/tracker/MkTracker.fun collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
