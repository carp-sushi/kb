.PHONY: all format build test lint run clean watch

all: format test

# The formatter mangles esqueleto queries, so skip formatting that file.
HS_FILES = $(filter-out src/Query.hs, $(shell find app/**/*.hs src -name "*.hs"))

format:
	@fourmolu -q -i $(HS_FILES)

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/*.hs app/server/*.hs app/migrator/*.hs

run:
	@stack run -- kb-server config/dev/settings

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	ghciwatch --clear --before-reload-shell "make format"
