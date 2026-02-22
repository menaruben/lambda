BUILD=build
SRC=src
SOURCES=$(SRC)/Main.hs $(SRC)/Parser.hs $(SRC)/Tokenizer.hs
OUT=lambda

.PHONY: build clean always

build: always
	ghc -o $(BUILD)/$(OUT) $(SOURCES)

clean:
	rm -r $(BUILD)/*

always:
	mkdir -p build
