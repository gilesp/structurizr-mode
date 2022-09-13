#!/bin/sh

emacs -batch \
      -l ert \
      -l ../structurizr-mode.el \
      -l unit-test.el \
      --eval '(ert-run-tests-batch-and-exit (not (tag ignored)))'
