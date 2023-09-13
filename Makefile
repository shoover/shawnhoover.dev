# Usage:
#   make build
#   make serve &
#   make deploy

# Pass force=t to force publishing all orgmode files regardless of cache status.

ifeq ($(force),)
force := nil
endif

.PHONY: build clean build-dir build-static build-notes-orgmode serve deploy

all: build

build: build-static build-notes-orgmode

build-dir:
	mkdir -p build/

build-static: | build-dir
	rsync -av --exclude ".#*" site/ build/

build-notes-orgmode: | build-dir
	emacs --script publish/publish-org-dir.el -- --force=$(force)

# The deploy script needs some environment variables, e.g. `source .env_deploy_sample`.
deploy:
	script/deploy-aws.sh

clean:
	mkdir -p build/
	rm -f -r build/*
	rm -f notes/index.org

serve:
	python3 -m http.server --directory=build 8011
