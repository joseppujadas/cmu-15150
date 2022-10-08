FILES = code/signature/TREE.sig code/signature/FOREST.sig code/books/book/Book.sml code/books/test/MkTest.fun code/books/student/Polly.sml code/books/student/MkStudent.fun code/books/student/Honk.sml code/books/reader/MkReader.fun code/books/adv-student/MkNemesis.fun code/books/adv-student/MkCompose.fun code/books/adv-student/Reset.sml code/books/Find150.sml collab.txt

.PHONY: all clean


all: handin.zip

clean:
	rm -f handin.zip

handin.zip: $(FILES)
	zip $@ $^
