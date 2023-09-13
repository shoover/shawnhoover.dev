# Usage: make build
#
# Pass force=t to force publishing all orgmode files regardless of cache status.

ifeq ($(force),)
force := nil
endif

.PHONY: build
build: build-static build-notes-orgmode

.PHONY: clean
clean:
	mkdir -p build/
	rm -f -r build/*
	rm -f notes/index.org

.PHONY: build-dir
build-dir:
	mkdir -p build/

.PHONY: build-static
build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

.PHONY: build-notes-orgmode
build-notes-orgmode: | build-dir
	emacs --script publish/publish-org-dir.el -- --force=$(force)

.PHONY: serve
serve:
	python3 -m http.server --directory=build 8011
