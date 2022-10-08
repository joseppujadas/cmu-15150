FILES = code/detypify/types.sml code/findn/findn.sml code/sat/sat.sml code/wontStarve/findPartition.sml code/wontStarve/wontStarve.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
