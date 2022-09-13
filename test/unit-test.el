(require 'ert)
(require 'structurizr-mode)

;; Shamelessly "inspired" by https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/blob/master/test/groovy-unit-test.el
(defmacro should-indent-to (source result)
  "Assert that SOURCE is indented to produce RESULT."
  `(with-temp-buffer
     (insert ,source)
     (structurizr-mode)
     (setq indent-tabs-mode nil)
     (indent-region (point-min) (point-max))
     (should (equal (buffer-substring-no-properties (point-min) (point-max))
                    ,result))))

(defmacro should-preserve-indent (source)
  "Assert that SOURCE does not change when indented."
  (let ((src-sym (make-symbol "src")))
    `(let ((,src-sym ,source))
       (should-indent-to ,src-sym ,src-sym))))

;; Smoke Test
(ert-deftest structurizr-smoke-test ()
  "Ensure that we can activate the structurizr major mode."
  (with-temp-buffer
    (structurizr-mode)))

;; Indentation Tests
(ert-deftest structurizr-indent-function ()
  "We should indent according to the number of curly braces."
  (should-indent-to
   "workspace {
model {
s = softwareSystem \"Software System\" {
webapp = container \"Web Application\"
}
}
}"

  "workspace {
    model {
        s = softwareSystem \"Software System\" {
            webapp = container \"Web Application\"
        }
    }
}"))

(ert-deftest structurizr-indent-function-handles-single-line-comments ()
  "Single line comments should be handled."
  (should-preserve-indent
   "workspace {
    // a comment
    model {
    }
}"))


(ert-deftest structurizr-indent-handles-curly-brace-in-comments ()
  "Curly braces should be ignored in comments."
  :tags '(ignored)
  (should-preserve-indent
   "workspace {
    // a comment with a brace {
    model {
    }
}"))

(ert-deftest structurizr-indent-function-handles-multi-line-comments ()
  "Multi line comments should be handled."
  (should-preserve-indent
   "workspace {
    model {
        /*
        blah
        */
    }
}"))
;; TODO: test that our mode hook gets called.
