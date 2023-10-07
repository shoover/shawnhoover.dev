#!/bin/bash

# Checks that emacs is install and sets up package dependencies.

file --dereference `which emacs`
emacs --version

emacs --batch --eval "(package-install 'htmlize)"
