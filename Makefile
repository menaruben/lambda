BUILD=build
SRC=src
SOURCES=$(SRC)/Main.hs $(SRC)/Parser.hs $(SRC)/Tokenizer.hs $(SRC)/Preprocessor.hs
FLAGS=-odir $(BUILD) -hidir $(BUILD) -i$(SRC)
OUT=lambda

.PHONY: build clean always

build: always
	ghc -o $(BUILD)/$(OUT) $(SOURCES) $(FLAGS)

clean:
	[ -d $(BUILD) ] && find $(BUILD) -mindepth 1 -delete || true

always:
	mkdir -p build
