(require 'ert)
(require 'shut-up)
(require 'structurizr-mode)

(ert-deftest structurizr-smoke-test ()
  "Ensure that we can activate the structurizr major mode."
  (with-temp-buffer
    (structurizr-mode)))

;; TODO: test that our mode hook gets called.
