FILES = code/interpreter/interpreter.sml code/universal/encryption.sml code/universal/bank.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
