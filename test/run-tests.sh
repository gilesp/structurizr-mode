#!/bin/sh

emacs -batch \
      -l ert \
      -l ../structurizr-mode.el \
      -l unit-test.el \
      -f ert-run-tests-batch-and-exit
