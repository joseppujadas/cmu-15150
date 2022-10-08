FILES = code/bbs/RefRand.sml code/array/ArrayFn.sml code/array/MemoFib.fun collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
