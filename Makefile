PWSH = pwsh

.PHONY: build
build: build-deps
	cabal v2-build exe:kb

.PHONY: build-deps
build-deps:
	cabal v2-build --only-dependencies exe:kb

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse app | ForEach-Object { stylish-haskell -i $$_.FullName } }"

.PHONY: setup-format
setup-format:
	cabal v2-install stylish-haskell --overwrite-policy=always

.PHONY: lint
lint:
	hlint app

.PHONY: setup-lint
setup-lint:
	cabal v2-install hlint --overwrite-policy=always

.PHONY: setup
setup: setup-format setup-lint

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: clean
clean:
	cabal v2-clean
