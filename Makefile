.PHONY: clean
clean:
	mkdir -p build/
	rm -f -r build/*

.PHONY: build
build:
	mkdir -p build/
	rsync -av --exclude ".#*" site/ build/
	emacs --script publish/publish-org-dir.el
