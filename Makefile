# Usage:
#   make build
#   make serve &
#   make deploy

# Pass force=t to force publishing all orgmode files regardless of cache status.
# Pass dev=t to build in dev mode, i.e. enable hot reloading.

ifeq ($(force),)
force := nil
endif

ifeq ($(dev),)
dev := nil
endif

.PHONY: browse build build-dir build-notes-orgmode build-static clean clean-publish-cache
.PHONY: deploy serve

all: build

build: build-static build-notes-orgmode

build-dir:
	mkdir -p build/

build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

build-notes-orgmode: | build-dir
	emacs --script publish/publish.el -- --force=$(force) --dev=$(dev)

# The deploy script needs some environment variables, e.g. `source .env_deploy_sample`.
deploy:
	script/deploy-aws.sh

clean:
	mkdir -p build/
	rm -f -r build/*
	rm -f notes/index.org

# Reset the global org publish cache, typically in ~/.org-timestamps. It's a blunt tool
# for when sitemap headlines get cached and don't update.
clean-publish-cache:
	emacs --batch --eval "(progn (require 'ox-publish) (org-publish-remove-all-timestamps))"

serve:
	cd publish; bundle exec ruby server.rb

browse:
	open http://127.0.0.1:5000/notes
