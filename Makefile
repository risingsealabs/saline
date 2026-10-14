root = $(shell pwd)

# Can only import from first specified package
# https://github.com/haskell/cabal/pull/9202#issuecomment-1692143024
# https://github.com/haskell/cabal/issues/10228
repl = cabal repl dev all

define ghcid
	ghcid                                       \
		-c '$(repl)'                              \
		--warnings                                \
		--restart 'common/common.cabal'           \
		--restart 'backend/backend.cabal'         \
		--restart 'frontend/frontend.cabal'       \
		--restart 'saline-node/saline-node.cabal' \
		--restart 'dev/dev.cabal'
endef

define ghcid-test
	$(ghcid)                           \
	--restart 'config'
endef

#
# Call the following inside `nix-shell`
#

.PHONY: all
all: build

.PHONY: hoogle
hoogle:
	hoogle server --local -p 8080

.PHONY: build
build:
	cabal build all

.PHONY: run
run:
	cabal run frontend

.PHONY: repl
repl:
	$(repl)

.PHONY: watch
watch:
	$(ghcid)

.PHONY: test
test:
	$(ghcid-test)                      \
	--test 'Saline.Test.dev'

.PHONY: dev
dev:
	$(ghcid-test)                      \
	--test 'Saline.Dev.dev "$(root)"'

.PHONY: sdk-python
sdk-python:
	$(ghcid-test)                      \
	--test 'Saline.Integrations.Python.dev "$(root)"'

#
# Call the following outside `nix-shell`
#

.PHONY: android
android:
	nix-build --argstr system "x86_64-linux" -A android.frontend

.PHONY: iOS
iOS:
	nix-build --argstr system "x86_64-darwin" -A ios.frontend
