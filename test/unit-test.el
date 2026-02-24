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

;; Comment syntax tests
(ert-deftest structurizr-comment-variables-are-set ()
  "Comment variables should be configured for hideshow support."
  (with-temp-buffer
    (structurizr-mode)
    (should (equal comment-start "// "))
    (should (equal comment-end ""))))

(ert-deftest structurizr-hash-comments-are-recognized ()
  "Hash comments should be recognized by the syntax table."
  (with-temp-buffer
    (structurizr-mode)
    (insert "# this is a comment\nworkspace")
    (goto-char (point-min))
    (forward-char 1)
    (should (nth 4 (syntax-ppss)))))  ; nth 4 means "in a comment"

(ert-deftest structurizr-slash-comments-are-recognized ()
  "Double-slash comments should be recognized by the syntax table."
  (with-temp-buffer
    (structurizr-mode)
    (insert "// this is a comment\nworkspace")
    (goto-char (point-min))
    (forward-char 3)
    (should (nth 4 (syntax-ppss)))))

(ert-deftest structurizr-multiline-comments-are-recognized ()
  "Multi-line comments should be recognized by the syntax table."
  (with-temp-buffer
    (structurizr-mode)
    (insert "/* this is\na comment */\nworkspace")
    (goto-char (point-min))
    (forward-char 3)
    (should (nth 4 (syntax-ppss)))))

;; Hideshow support tests
(ert-deftest structurizr-hideshow-is-configured ()
  "Hideshow should be configured for structurizr-mode."
  (require 'hideshow)
  (should (assoc 'structurizr-mode hs-special-modes-alist)))

(ert-deftest structurizr-hideshow-can-be-enabled ()
  "Hideshow minor mode should be able to activate without error."
  (with-temp-buffer
    (structurizr-mode)
    (hs-minor-mode 1)
    (should hs-minor-mode)))

;; Outline mode support tests
(ert-deftest structurizr-outline-regexp-is-set ()
  "Outline regexp should be configured for keyword-based folding."
  (with-temp-buffer
    (structurizr-mode)
    (should (boundp 'outline-regexp))
    (should outline-regexp)))

(ert-deftest structurizr-outline-level-is-set ()
  "Outline level function should be configured."
  (with-temp-buffer
    (structurizr-mode)
    (should (boundp 'outline-level))
    (should (functionp outline-level))))

;; TODO: test that our mode hook gets called.
