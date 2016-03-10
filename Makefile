
.PHONY: test

ELM_COMPILE=elm-make --yes --warn

ELM_IO = elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh

TESTS=$(wildcard tests/*.elm)
TESTS_JS=$(patsubst %.elm,%.elm.test.js,$(TESTS))

TARGETS=$(wildcard *.elm Arch/*.elm)
TARGETS_JS=$(patsubst %.elm,%.js,$(TARGETS))

DEMO = demo/PythagorasTree.html demo/Serpinski.html demo/Picture.html demo/Animation.html

all: compile

compile: $(TARGETS_JS)

%.js: %.elm
	@echo 'Compile $(ELM_COMPILE)  $<'
	$(ELM_COMPILE) $< --output $@

demo: $(DEMO)

demo/%.html: %.elm
	elm-make --yes $< --output $@


test: $(TESTS_JS)

%.elm.test.js: %.elm
	elm-make --yes $< --output tests/raw.test.js
	$(ELM_IO) tests/raw.test.js $<.test.js
	node $<.test.js && rm $<.test.js tests/raw.test.js

clean-test:
	-@rm $(TESTS_JS) test/raw.test.js || true

clean: clean-test
	-@rm $(TARGETS_JS) || true
