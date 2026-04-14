;;; org-grimoire-tests.el --- Unit tests for org-grimoire.el -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Dennis Burgermeister <dennis@dencla.de>
;; Maintainer: Dennis Burgermeister <dennis@dencla.de>
;; URL: https://github.com/denclade/org-grimoire

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for org-grimoire.el.  Note that we are using Shorthands in
;; this file, so the "ogt-" prefix really is "org-grimoire-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'org-grimoire
         (expand-file-name "org-grimoire.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

;; Dummy data

(defconst ogt--test-dummy-config
  '(:site-title "My Blog" :per-page 10)
  "Dummy configuration data for running tests.")

(defmacro with-ogt-config (&rest body)
  "Evaluate BODY with `org-grimoire--config' bound to dummy data."
  (declare (indent 0) (debug t))
  `(let ((org-grimoire--config (copy-sequence ogt--test-dummy-config)))
     ,@body))

;;; Fixture Setup:

(defvar ogt-base-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The base directory in which the tests are executed.")

(defmacro with-ogt-fixture (fixture-name &rest body)
  "Execute BODY, the working directory is set to FIXTURE-NAME."
  (declare (indent 1) (debug t))
  `(let* ((fixture-dir (expand-file-name (concat "fixtures/" ,fixture-name "/")
                                         ogt-base-dir))
          (default-directory fixture-dir))
     
     (unless (file-directory-p fixture-dir)
       (error "Fixture folder missing! Did you create '%s'?" fixture-dir))
     
     ,@body))

;;; Messages setup to test logging
(defmacro with-captured-messages (var-name &rest body)
  "Execute BODY and get all `message'-calls in the list VAR-NAME.
The messages are stored in order of printing."
  (declare (indent 1) (debug t))
  `(let ((,var-name '()))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq ,var-name (append ,var-name (list (apply #'format fmt args)))))))
       ,@body)))

;;; Temp folders
(defmacro with-ogt-temp-dirs (vars &rest body)
  "Create temporary directories bound to VARS, execute BODY, and clean up.
Example result:

    (let ((src-dir (make-temp-file 'org-grimoire-test-' t))
          (out-dir (make-temp-file 'org-grimoire-test-' t)))
      ;; @body code
    )"
  (declare (indent 1))
  `(let ,(mapcar (lambda (var) `(,var (make-temp-file "org-grimoire-test-" t))) vars)
     ;; Ensures to delete the created directories
     (unwind-protect
         (progn ,@body)
       ,@(mapcar (lambda (var) `(delete-directory ,var t)) vars))))

;; org-grimoire--config-get

(ert-deftest ogt-config-get-existing ()
  "Test that `org-grimoire--config-get' does return existing settings correctly."
  (with-ogt-config
    (should (equal (org-grimoire--config-get :site-title) "My Blog"))
    (should (equal (org-grimoire--config-get :per-page) 10))))

(ert-deftest ogt-config-get-missing-key ()
  "Test that `org-grimoire--config-get' does return nil for non-existing settings correctly."
  (with-ogt-config
    (should (equal (org-grimoire--config-get :theme) nil))))

(ert-deftest org-grimoire-config-get-nil-config-test ()
  "Test that `org-grimoire--config-get' does return an error if no config is loaded."
  (let ((org-grimoire--config nil))
    (should-error (org-grimoire--config-get :site-title) :type 'error)))

;; org-grimoire--config-load

(ert-deftest ogt-load-config-valid ()
  "Test that `org-grimoire--config-load' does load the correct config in an grimoire folder."
  (with-ogt-fixture "valid-site"
    (let ((loaded-config (org-grimoire--load-config)))
      (should (equal (plist-get loaded-config :site-title) "Fixture Blog")))))

(ert-deftest ogt-load-config-base-dir-added ()
  "Test that `org-grimoire--config-load' does load the correct config in an grimoire folder."
  (with-ogt-fixture "valid-site"
    (let ((loaded-config (org-grimoire--load-config)))
      (should (equal (plist-get loaded-config :base-dir) default-directory)))))

(ert-deftest ogt-load-config-missing ()
  "Test that `org-grimoire--config-load' does throw an error if no config can be found."
  (with-ogt-fixture "not-an-grimoire-folder"
    (should-error (org-grimoire--load-config) :type 'user-error)))

;; Logging stuff

(ert-deftest ogt-log-reset-successfull ()
  "Test that `org-grimoire--log-reset' does what it is supposed to do."
  (setq org-grimoire--log '((:info . "Old info message") (:error . "Old error")))

  (org-grimoire--log-reset)

  (should (equal org-grimoire--log nil)))

(ert-deftest ogt-log-info ()
  "Test that the function `org-grimoire--log' correctly logs infos."
  (setq org-grimoire--log nil)

  (with-captured-messages messages
    (let ((info-text "Found 3 files."))
      (org-grimoire--log :info info-text)
      (should (equal (nth 0 messages) (format "[INFO] %s" info-text)))
      (should (equal org-grimoire--log `((:info . ,info-text)))))))

(ert-deftest ogt-log-warning ()
  "Test that the function `org-grimoire--log' correctly logs warnings."
  (setq org-grimoire--log nil)

  (with-captured-messages messages
    (let ((warn-text "Missing tag."))
      (org-grimoire--log :warn warn-text)
      (should (equal (nth 0 messages) (format "[WARN] %s" warn-text)))
      (should (equal org-grimoire--log `((:warn . ,warn-text)))))))

(ert-deftest ogt-log-error ()
  "Test that the function `org-grimoire--log' correctly logs errors."
  (setq org-grimoire--log nil)

  (with-captured-messages messages
    (let ((error-text "Template not found!"))
      (org-grimoire--log :error error-text)
      (should (equal (nth 0 messages) (format "[ERROR] %s" error-text)))
      (should (equal org-grimoire--log `((:error . ,error-text)))))))

(ert-deftest ogt-log-append ()
  "Test that the function `org-grimoire--log' correctly appends additional messages."
  (setq org-grimoire--log nil)

  (with-captured-messages messages
    (let ((info-text "Found 3 files.")
          (info-text2 "Found 4 static files.")
          (warn-text "Missing tag.")
          )
      (org-grimoire--log :info info-text)
      (org-grimoire--log :info info-text2)
      (org-grimoire--log :warn warn-text)
      
      (should (equal (nth 0 messages) (format "[INFO] %s" info-text)))
      (should (equal (nth 1 messages) (format "[INFO] %s" info-text2)))
      (should (equal (nth 2 messages) (format "[WARN] %s" warn-text)))
      (should (equal org-grimoire--log `((:warn . ,warn-text)
                                         (:info . ,info-text2)
                                         (:info . ,info-text)))))))

(ert-deftest ogt-log-summary-no-warn-error ()
  "Test that `org-grimoire--log-summary' correctly outputs when there are no errors or warnings."
  (setq org-grimoire--log nil)
  (org-grimoire--log :info "Just some normal info.")
  
  (with-captured-messages messages
    (org-grimoire--log-summary)
    
    (should (= (length messages) 1))
    (should (equal (car messages) "Build completed with no warnings or errors."))))

(ert-deftest ogt-log-summary-with-warn ()
  "Test that `org-grimoire--log-summary' shows errors or warnings if they are present."
  (setq org-grimoire--log nil)
  (org-grimoire--log :warn "Missing tag")
  
  (with-captured-messages messages
    (org-grimoire--log-summary)
    
    (should (= (length messages) 3))
    (should (equal (nth 0 messages) "--- Build Summary ---"))
    (should (equal (nth 1 messages) "  [WARN ] Missing tag"))
    (should (equal (nth 2 messages) "  1 warning(s), 0 error(s)."))))

(ert-deftest ogt-log-summary-with-error ()
  "Test that `org-grimoire--log-summary' shows errors or warnings if they are present."
  (org-grimoire--log-reset)
  (org-grimoire--log :error "Failed to render")
  
  (with-captured-messages messages
    (org-grimoire--log-summary)
    
    (should (= (length messages) 3))
    (should (equal (nth 0 messages) "--- Build Summary ---"))
    (should (equal (nth 1 messages) "  [ERROR] Failed to render"))
    (should (equal (nth 2 messages) "  0 warning(s), 1 error(s)."))))

(ert-deftest ogt-log-summary-with-warn-error ()
  "Test that `org-grimoire--log-summary' shows errors or warnings if they are present."
  (org-grimoire--log-reset)
  (org-grimoire--log :info "Start")
  (org-grimoire--log :warn "Missing tag")
  (org-grimoire--log :error "Failed to render")
  
  (with-captured-messages messages
    (org-grimoire--log-summary)
    
    (should (= (length messages) 4))
    (should (equal (nth 0 messages) "--- Build Summary ---"))
    (should (equal (nth 1 messages) "  [WARN ] Missing tag"))
    (should (equal (nth 2 messages) "  [ERROR] Failed to render"))
    (should (equal (nth 3 messages) "  1 warning(s), 1 error(s)."))))

;; Templates
(ert-deftest ogt-default-theme-dir ()
  "Test that `org-grimoire--default-theme-dir' points to the correct theme dir (with and without cfg override)."
  (with-ogt-config
    (with-ogt-fixture "template-site"
      (let ((org-grimoire--package-dir (expand-file-name "../" ogt-base-dir)))
        
        (should (string-suffix-p "themes/default/" (org-grimoire--default-theme-dir)))
        
        (let ((org-grimoire--config (plist-put org-grimoire--config :default-theme-dir "my-base/")))
          (should (string-suffix-p "template-site/my-base/" (org-grimoire--default-theme-dir))))))))

(ert-deftest ogt-resolve-theme-file-test ()
  "Test that `org-grimoire--resolve-theme-file' returns the correct child or default theme file."
  (with-ogt-config
    (with-ogt-fixture "template-site"
      (let ((org-grimoire--package-dir (expand-file-name "../" ogt-base-dir))
            (user-theme (expand-file-name "themes/custom-theme/" default-directory)))
        
        ;; Returns a user theme file
        (should (string-prefix-p user-theme (org-grimoire--resolve-theme-file "post.html" user-theme)))
        
        ;; Returns a default theme file if file is not present in user theme.
        (should (string-match-p "themes/default/tags.html$" (org-grimoire--resolve-theme-file "tags.html" nil)))
        
        ;; File does not exist.
        (should (equal nil (org-grimoire--resolve-theme-file "missing.html" user-theme)))))))

(ert-deftest ogt-load-template-test ()
  "Test that `org-grimoire--load-template' correctly loads a template and throws an error if the template does not exist."
  (with-ogt-config
    (with-ogt-fixture "template-site"
      (let ((org-grimoire--package-dir (expand-file-name "../" ogt-base-dir))
            (user-theme (expand-file-name "themes/custom-theme/" default-directory)))
        
        (should (equal "<h1>Index</h1>" (string-trim (org-grimoire--load-template "index" user-theme))))

        ;; error case - template does not exist
        (org-grimoire--log-reset)
        (with-captured-messages messages
          (let ((result (org-grimoire--load-template "not-existing-template" user-theme)))
            (should (equal result "<!-- Template not found: not-existing-template.html -->"))
            (should (equal org-grimoire--log '((:error . "Template not found: not-existing-template.html"))))
            (should (equal (nth 0 messages) "[ERROR] Template not found: not-existing-template.html"))))))))

(ert-deftest ogt-process-includes ()
  "Tests that `org-grimoire--process-includes' correctly includes file content in another file."
  (with-ogt-config
  (with-ogt-fixture "template-site"
    (let ((org-grimoire--package-dir (expand-file-name "../" ogt-base-dir))
          (user-theme (expand-file-name "themes/custom-theme/" default-directory)))
      
      ;; include an existing file
      (let* ((template "Before -> {{include post.html}} <- After")
             (result (org-grimoire--process-includes template user-theme)))
        (should (string-match-p "<- After" result))
        (should-not (string-match-p "{{include" result)))
      
      ;; missing Include
      (org-grimoire--log-reset)
      (with-captured-messages messages
        (let* ((template "Header {{include missing.html}} Footer")
               (result (org-grimoire--process-includes template user-theme)))
          
          (should (equal result "Header  Footer"))
          (should (equal (nth 0 messages) "[ERROR] Include not found: missing.html"))
          (should (equal org-grimoire--log '((:error . "Include not found: missing.html"))))))))))

(ert-deftest ogt-render-template ()
  "Tests that `org-grimoire--render-template' correctly replaces {{variable}}."
  (with-ogt-config
    (with-ogt-fixture "template-site"
      (let ((org-grimoire--package-dir (expand-file-name "../" ogt-base-dir))
            (user-theme (expand-file-name "themes/custom-theme/" default-directory))
            (template "<h1>{{title}}</h1><p>{{content}}</p>")
            (vars '(:title "Test" :content "Inhalt")))
        
        (should (equal (org-grimoire--render-template template vars user-theme)
                       "<h1>Test</h1><p>Inhalt</p>"))))))

(ert-deftest ogt-wrap-base ()
  "Tests that `org-grimoire--wrap-base' correctly loads the base template theme."
  (with-ogt-config
    (with-ogt-fixture "template-site"
      (let* ((org-grimoire--package-dir (expand-file-name "../" ogt-base-dir))
             (user-theme (expand-file-name "themes/custom-theme/" default-directory)))
        
        (let ((result (org-grimoire--wrap-base "<p>Post</p>" "Mein Post")))
          (should (string-match-p "Mein Post" result))
          (should (string-match-p "<p>Post</p>" result)))))))

(ert-deftest ogt-wrap-base-custom-vars-test ()
  "Tests that `org-grimoire--wrap-base' correctly inserts custom variables."
  (let ((org-grimoire--config (list :site-title "My Blog"
                                    :theme "dummy-theme"
                                    :my-custom-global "Global variable")))
    
    (cl-letf (((symbol-function 'org-grimoire--load-template)
               (lambda (name theme)
                 (should (equal name "base"))
                 (should (equal theme "dummy-theme"))
                 "<body>{{content}} | {{my-custom-global}} | {{page-var}}</body>")))
      
      (let* ((extra-vars '(:page-var "local variable"))
             (result (org-grimoire--wrap-base "Text" "Titel" nil extra-vars)))
        
        (should (string-search "Text | Global variable | local variable" result))))))

(ert-deftest ogt-wrap-base-title-test ()
  "Tests that `org-grimoire--wrap-base' correctly sets the site-title & a custom title."
  (with-ogt-config
    ;; simulate an empty template with just title stuff
    (cl-letf (((symbol-function 'org-grimoire--load-template)
               (lambda (_name _theme)
                 "<head><title>{{site-title}} :: {{title}}</title></head><body>{{content}}</body>")))
      
      (let* ((test-title "Emacs SSG!")
             (test-text "<p>My text about emacs SSG.</p>")
             (result (org-grimoire--wrap-base test-text test-title)))
        
        (should (string-match-p (format "<title>My Blog :: %s</title>" test-title) result))
        (should (string-match-p (format "<body>%s</body>" test-text) result))))))

;; File handling

(ert-deftest ogt-copy-static-nil ()
  "Tests that `org-grimoire--copy-static' does log a warning if src-dir is nil."
  (with-ogt-temp-dirs (dst-dir)
    (with-captured-messages messages
      (org-grimoire--copy-static nil dst-dir)
      (should (= (length messages) 1))
      (should (equal (nth 0 messages) "[WARN] Static dir nil does not exist, did not copy any files.")))))

(ert-deftest ogt-copy-static-not-existing ()
  "Tests that `org-grimoire--copy-static' does log a warning if it should copy from a dir which does not exist."
  (with-ogt-temp-dirs (dst-dir)
    (with-captured-messages messages
      
      (org-grimoire--copy-static "/does/not/exist" dst-dir)

      (should (= (length messages) 1))
      (should (equal (nth 0 messages) "[WARN] Static dir /does/not/exist does not exist, did not copy any files.")))))

(ert-deftest ogt-copy-static-success ()
  "Tests that `org-grimoire--copy-static' does correctly copies files if the src and dst dirs exist."
  (with-ogt-temp-dirs (src-dir dst-dir)
      (with-captured-messages messages
    (let ((sub-dir (expand-file-name "css" src-dir)))
      (make-directory sub-dir t)
      (write-region "hello" nil (expand-file-name "test.txt" src-dir))
      (write-region "body { color: red; }" nil (expand-file-name "style.css" sub-dir)))

    (org-grimoire--copy-static src-dir dst-dir)

    (should (= (length messages) 1))
    (should (equal (nth 0 messages) (format "[INFO] Copied 2 static file(s) to %s." dst-dir))))
    
    (should (file-exists-p (expand-file-name "test.txt" dst-dir)))
    (should (file-exists-p (expand-file-name "css/style.css" dst-dir)))))

(ert-deftest ogt-copy-theme-static-no-static-folder ()
  "Tests that `org-grimoire--copy-static' does not call copy-static if no static dir is in theme-dir."
  (with-ogt-temp-dirs (out-dir theme-dir)
    
    (cl-letf (((symbol-function 'org-grimoire--copy-static)
               (lambda (_src _dst)
                 (ert-fail "org-grimoire--copy-static called with not existing folder!"))))
      
      (org-grimoire--copy-theme-static out-dir theme-dir))))

(ert-deftest ogt-copy-theme-static-success ()
  "Tests that `org-grimoire--copy-static' does correctly copy a static folder in theme-dir."
  (with-ogt-temp-dirs (theme-dir dst-dir)
    (let ((theme-static (expand-file-name "static" theme-dir))
          (was-called nil))
      (make-directory theme-static t)
      (cl-letf (((symbol-function 'org-grimoire--copy-static)
                 (lambda (src dst)
                   (setq was-called t)
                   (should (equal src theme-static))
                   (should (equal dst (expand-file-name "static" dst-dir))))))

        (org-grimoire--copy-theme-static dst-dir theme-dir)
        (should was-called)))))
      
(provide 'org-grimoire-test)
;;; org-grimoire-test.el ends here
