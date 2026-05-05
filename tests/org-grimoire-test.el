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

;;; Collect

(ert-deftest ogt-extract-keyword-nil ()
  "Tests that `org-grimoire--extract-keyword' correctly finds #+KEYWORDS."
  (with-temp-buffer
    (insert "#+DATE: 2023-10-01\n#+DRAFT: t\nTITLE: this is just in text.")
    (org-mode)
    (let ((ast (org-element-parse-buffer)))
      (should (equal (org-grimoire--extract-keyword ast "TITLE") nil)))))

(ert-deftest ogt-extract-keyword-success ()
  "Tests that `org-grimoire--extract-keyword' correctly finds #+KEYWORDS."
  (with-temp-buffer
    (insert "#+TITLE: Hello World\n#+DATE: 2023-10-01\n#+DRAFT: t")
    (org-mode)
    (let ((ast (org-element-parse-buffer)))
      (should (equal (org-grimoire--extract-keyword ast "TITLE") "Hello World"))
      (should (equal (org-grimoire--extract-keyword ast "DRAFT") "t"))
      (should (null (org-grimoire--extract-keyword ast "MISSING"))))))

(ert-deftest ogt-reading-time-from-ast-success ()
  "Tests the reading time calculation based on word count."
  (with-temp-buffer
    (insert "#+TITLE: Ignore this completely, longer title, ...\nOne two three four five six seven eight nine ten.")
    (org-mode)
    (let ((ast (org-element-parse-buffer)))
      ;; 10 words / 200 WPM roundet to 0 -> 0 min
      (should (equal (org-grimoire--reading-time-from-ast ast) "0 min read"))
      ;; 10 words / 19 WPM roundet to 1 -> 1 min
      (should (equal (org-grimoire--reading-time-from-ast ast 19) "1 min read"))
      ;; 10 words / 20 WPM roundet to 0 -> 0 min
      (should (equal (org-grimoire--reading-time-from-ast ast 20) "0 min read"))
      ;; 20 words / 2 WPM -> = 5 Min)
      (should (equal (org-grimoire--reading-time-from-ast ast 2) "5 min read")))))

(ert-deftest ogt-reading-time-from-ast-zero-words ()
  "Tests the reading time calculation based on word count."
  (with-temp-buffer
    (insert "")
    (org-mode)
    (let ((ast (org-element-parse-buffer)))
      (should (equal (org-grimoire--reading-time-from-ast ast 1) "0 min read")))))

;;; --- Utility Tests ---

(ert-deftest ogt-file-to-slug ()
  "Tests that `org-grimoire--file-to-slug' strips path and extension."
  (should (equal (org-grimoire--file-to-slug "/var/www/site/my-cool-post.org") "my-cool-post"))
  (should (equal (org-grimoire--file-to-slug "just_a-name.org") "just_a-name")))

(ert-deftest ogt-parse-tags ()
  "Tests that `org-grimoire--parse-tags' parses various tag formats in addition to the default FILETAGS format."
  ;; the actual correct one
  (should (equal (org-grimoire--parse-tags ":elisp:emacs:org:") '("elisp" "emacs" "org")))
  ;; with fallbacks
  (should (equal (org-grimoire--parse-tags "elisp:emacs:org") '("elisp" "emacs" "org")))
  (should (equal (org-grimoire--parse-tags "elisp, emacs org") '("elisp" "emacs" "org")))
  (should (equal (org-grimoire--parse-tags "elisp emacs org") '("elisp" "emacs" "org")))
  (should (equal (org-grimoire--parse-tags ":singleTag:") '("singleTag")))
  (should (equal (org-grimoire--parse-tags "singleTag,") '("singleTag")))
  (should (equal (org-grimoire--parse-tags "singleTag, ") '("singleTag")))
  (should (equal (org-grimoire--parse-tags "singleTag") '("singleTag")))
  (should (null (org-grimoire--parse-tags nil))))

(ert-deftest ogt-normalize-boolean ()
  "Tests that `org-grimoire--normalize-boolean' normalizes t, True and yes to boolean t."
  (should (equal (org-grimoire--normalize-boolean "t") t))
  (should (equal (org-grimoire--normalize-boolean "T") t))
  (should (equal (org-grimoire--normalize-boolean "True") t))
  (should (equal (org-grimoire--normalize-boolean "true") t))
  (should (equal (org-grimoire--normalize-boolean "TRUE") t))
  (should (equal (org-grimoire--normalize-boolean "YES") t))
  (should (equal (org-grimoire--normalize-boolean "yes") t))
  ;; Negatives
  (should (equal (org-grimoire--normalize-boolean "no") nil))
  (should (equal (org-grimoire--normalize-boolean "false") nil))
  ;; Default Value Handling
  (should (equal (org-grimoire--normalize-boolean nil 'my-default) 'my-default))
  (should (equal (org-grimoire--normalize-boolean nil t) t)))

(ert-deftest ogt-infer-type ()
  "Tests that `org-grimoire--infer-type' inferrs the post type from the immediate subdirectory of source."
  (should (equal (org-grimoire--infer-type "/source/blog/my-post.org" "/source/") "blog"))
  (should (equal (org-grimoire--infer-type "/source/blog/2026/my-post.org" "/source/") "blog"))
  (should (equal (org-grimoire--infer-type "/source/pages/about.org" "/source/") "pages"))
  ;; No subdir, directly source
  (should (null (org-grimoire--infer-type "/source/index.org" "/source/"))))

;;; --- Collecting Assets & Files ---

(ert-deftest ogt-collect-assets ()
  "Tests that `org-grimoire--collect-assets' checks for assets and links are extracted and missing ones log a error."
  (let* ((temp-dir (make-temp-file "ogt-assets-" t))
         (img-path (expand-file-name "test-image.png" temp-dir))
         (source-file (expand-file-name "post.org" temp-dir)))
    
    (unwind-protect
        (progn
          (write-region "dummy" nil img-path)
          
          (with-temp-buffer
            (insert "Ein gültiges Bild: [[file:test-image.png]]\n")
            (insert "Ein fehlendes Bild: [[file:does-not-exist.jpg]]\n")
            (org-mode)
            
            (let ((ast (org-element-parse-buffer)))
              
              (with-captured-messages messages
                (let ((assets (org-grimoire--collect-assets ast source-file)))
                  
                  ;; Only the existing image ("test-image.png") is kept
                  (should (= (length assets) 1))
                  (should (equal (car assets) img-path))
                  
                  ;; Theres one error
                  (should (= (length messages) 1))
                  
                  ;; Check if it is an error
                  (should (equal (nth 0 messages) "[ERROR] Asset missing: 'does-not-exist.jpg' referenced in 'post.org'!")))))))
      
      (delete-directory temp-dir t))))

(ert-deftest ogt-collect-file ()
  "Tests that `org-grimoire--collect-file' collects all relevant properties from an file."
  (let* ((src-dir (make-temp-file "ogt-src-" t))
         (out-dir "/fake-out/")
         (blog-dir (expand-file-name "blog" src-dir))
         (org-file (expand-file-name "test-post.org" blog-dir)))
    
    (unwind-protect
        (progn
          (make-directory blog-dir t)
          (write-region "#+TITLE: My Post\n#+DATE: 2023-11-11\n#+FILETAGS: :emacs:lisp:\nHello!" nil org-file)
          
          (cl-letf (((symbol-function 'org-grimoire--config-get)
                     (lambda (key) (eq key :reading-time))))
            
            (let ((post (org-grimoire--collect-file org-file src-dir out-dir)))
              (should (equal (plist-get post :title) "My Post"))
              (should (equal (plist-get post :date) "2023-11-11"))
              (should (equal (plist-get post :type) "blog"))  ;; got from blog folder
              (should (equal (plist-get post :draft) nil))
              (should (equal (plist-get post :listed) t))
              (should (equal (plist-get post :tags) '("emacs" "lisp")))
              (should (equal (plist-get post :slug) "test-post"))
              (should (equal (plist-get post :reading-time) "0 min read"))
              (should (equal (plist-get post :output) "/fake-out/blog/test-post.html")))))
      
      (delete-directory src-dir t))))

;;; --- Pipeline Tests (Collecting many & Sorting) ---

(ert-deftest ogt-collect ()
  "Tests that `org-grimoire--collect' collects all valid files, ignoring drafts and typeless files."
  (cl-letf (((symbol-function 'directory-files-recursively)
             (lambda (_dir _regex) '("file1.org" "file2.org" "file3.org")))
            
            ;; return faked plists
            ((symbol-function 'org-grimoire--collect-file)
             (lambda (f _s _o)
               (cond ((equal f "file1.org") '(:title "Valid Post" :type "blog" :draft nil))
                     ((equal f "file2.org") '(:title "Draft Post" :type "blog" :draft t))
                     ((equal f "file3.org") '(:title "Typeless"   :type nil    :draft nil))))))
    
    (with-captured-messages messages
      (let ((posts (org-grimoire--collect "/src" "/out")))
        
        ;; Only file1.org should be there
        (should (= (length posts) 1))
        (should (equal (plist-get (car posts) :title) "Valid Post"))
        
        ;; Expected logs (1 Info für Draft, 1 Warnung für Typeless)
        (should (= (length messages) 2))
        
        (should (string-match-p "\\[INFO ?\\]" (nth 0 messages)))
        (should (string-search "Skipping draft: file2.org" (nth 0 messages)))
        
        (should (string-match-p "\\[WARN ?\\]" (nth 1 messages)))
        (should (string-search "No type directory, skipping: file3.org" (nth 1 messages)))))))

(ert-deftest ogt-sort-posts-by-date ()
  "Tests that `org-grimoire--sort-posts-by-date' sorts posts by date from newest to oldest."
  (let* ((post-old '(:title "Old" :date "2020-01-01"))
         (post-new '(:title "New" :date "2023-12-12"))
         (post-mid '(:title "Mid" :date "2022-05-05"))
         (post-nil '(:title "NoDate" :date nil))
         (posts (list post-old post-nil post-new post-mid)))
    
    (let ((sorted (org-grimoire--sort-posts-by-date posts)))
      (should (equal (plist-get (nth 0 sorted) :title) "New"))
      (should (equal (plist-get (nth 1 sorted) :title) "Mid"))
      (should (equal (plist-get (nth 2 sorted) :title) "Old"))
      (should (equal (plist-get (nth 3 sorted) :title) "NoDate")))))

;;; --- Rendering Tests ---

(ert-deftest ogt-org-to-html-successfull ()
  "Tests that `org-grimoire--org-to-html' successfully exports Org syntax to HTML body."
  (let* ((temp-file (make-temp-file "ogt-export-" nil ".org")))
    (unwind-protect
        (progn
          (write-region "Hello *bold* and /italic/." nil temp-file)
          
          (let ((html (org-grimoire--org-to-html temp-file)))
            (should (string-match-p "Hello" html))
            (should (or (string-match-p "<b>bold</b>" html)
                        (string-match-p "<strong>bold</strong>" html)))
            ;; There shouldn't be any head boilerplate as org-export-as is called with onlybody
            (should-not (string-match-p "<head>" html))))
      (delete-file temp-file))))

(ert-deftest ogt-tags-html-successfull ()
  "Tests that `org-grimoire--tags-html' creates correct links to tags inside a div with the correct classnames."

  ;; mock to only downcase as it is not important here.
  (cl-letf (((symbol-function 'org-grimoire--tag-to-slug) #'downcase))
    (let ((html (org-grimoire--tags-html '("Emacs" "Lisp"))))
      (should (string-search "<div class=\"grimoire-tags\">" html))
      ;; check slug + correct name
      (should (string-search "<a class=\"grimoire-tag\" href=\"/tags/emacs.html\">Emacs</a>" html))
      (should (string-search "<a class=\"grimoire-tag\" href=\"/tags/lisp.html\">Lisp</a>" html)))))

(ert-deftest ogt-tags-html-nil ()
  "Tests that `org-grimoire--tags-html' returns an empty string if called with no tags."
  (should (equal (org-grimoire--tags-html nil) "")))

(ert-deftest ogt-post-site-url-success ()
  "Tests that `org-grimoire--post-site-url' returns the correct root-relative URL."
  (cl-letf (((symbol-function 'org-grimoire--config-get)
             (lambda (key)
               (if (eq key :output) "/var/www/public/" nil))))
    
    (let ((post '(:output "/var/www/public/blog/my-post.html")))
      (should (equal (org-grimoire--post-site-url post) "/blog/my-post.html")))))

(ert-deftest ogt-render-post ()
  "Tests the orchestration of templates and HTML conversion for a single post."

  ;; mock everything to just test render-post
  (cl-letf (((symbol-function 'org-grimoire--config-get) (lambda (_) "dummy-theme"))
            ((symbol-function 'org-grimoire--load-template) (lambda (_type _theme) "TEMPLATE-STRING"))
            ((symbol-function 'org-grimoire--org-to-html) (lambda (_src) "<p>Content</p>"))
            ((symbol-function 'org-grimoire--tags-html) (lambda (_tags) "<tags>"))
            ((symbol-function 'org-grimoire--post-site-url) (lambda (_p) "/post.html"))
            
            ;; Just append the variables in render-template
            ((symbol-function 'org-grimoire--render-template)
             (lambda (_tmpl vars _theme)
               (format "Title:%s Content:%s" (plist-get vars :title) (plist-get vars :content))))
            
            ;; Add something via the base wrapper
            ((symbol-function 'org-grimoire--wrap-base)
             (lambda (inner title url)
               (format "[BASE url=%s] %s" url inner))))
    
    (let* ((post '(:type "blog" :title "My Title" :source "test.org"))
           (result (org-grimoire--render-post post)))
      
      (should (equal result "[BASE url=/post.html] Title:My Title Content:<p>Content</p>")))))

;;; --- Write & Asset Pipeline Tests ---

(ert-deftest ogt-copy-assets ()
  "Tests that assets are copied relative to the source directory."
  (with-ogt-temp-dirs (src-dir out-dir)
    (let ((sub-dir (expand-file-name "gfx" src-dir)))
      (make-directory sub-dir t)
      (let* ((img-file (expand-file-name "gfx/image.png" src-dir))
             (source-file (expand-file-name "post.org" src-dir))
             (output-file (expand-file-name "post.html" out-dir))
             (expected-dest (expand-file-name "gfx/image.png" out-dir)))
        
        (write-region "binary-data" nil img-file)
        
        (org-grimoire--copy-assets (list img-file) source-file output-file)
        
        (should (file-exists-p expected-dest))))))

(ert-deftest ogt-write-post ()
  "Tests that a post is rendered, written to disk, and assets are copied."
  (with-ogt-temp-dirs (out-dir)
    (let* ((out-file (expand-file-name "post.html" out-dir))
           (post `(:output ,out-file :source "dummy.org" :assets ("img.png")))
           (assets-copied nil))
      
      (cl-letf (((symbol-function 'org-grimoire--render-post)
                 (lambda (_p) "<html>MOCKED HTML</html>"))
                
                ((symbol-function 'org-grimoire--copy-assets)
                 (lambda (assets _src _out)
                   (should (equal assets '("img.png")))
                   (setq assets-copied t))))
        
        (with-captured-messages messages
          (org-grimoire--write-post post)
          
          (should (file-exists-p out-file))
          (should (equal (with-temp-buffer (insert-file-contents out-file) (buffer-string))
                         "<html>MOCKED HTML</html>"))
          (should assets-copied)
          (should (= (length messages) 1))
          (should (string-match-p "\\[INFO ?\\]" (nth 0 messages)))
          (should (string-search "Rendered:" (nth 0 messages))))))))

(ert-deftest ogt-render-all ()
  "Tests the render loop and ensures errors in single posts don't crash the loop."
  (let ((post-success '(:source "good.org"))
        (post-fail '(:source "bad.org"))
        (render-count 0))
    
    (cl-letf (((symbol-function 'org-grimoire--write-post)
               (lambda (post)
                 (cl-incf render-count)
                 (when (equal (plist-get post :source) "bad.org")
                   (error "Simulated rendering error")))))
      
      (with-captured-messages messages
        (org-grimoire--render-all (list post-success post-fail))
        
        (should (= render-count 2))
        
        (should (= (length messages) 1))
        (should (string-match-p "\\[WARN ?\\]" (nth 0 messages)))
        (should (string-search "Failed to render bad.org" (nth 0 messages)))
        (should (string-search "Simulated rendering error" (nth 0 messages)))))))

;; Index and pagination
(ert-deftest ogt-render-post-item ()
  "Tests that `org-grimoire--render-post-item' correctly formats a post item."
  (with-ogt-config
    
    ;; 1. Wir definieren leere Variablen, um die Daten "einzufangen"
    (let ((captured-template-name nil)
          (captured-vars nil))
      
      (cl-letf (((symbol-function 'org-grimoire--post-site-url)
                 (lambda (_p) "/posts/sample.html"))
                
                ((symbol-function 'org-grimoire--load-template)
                 (lambda (name _theme)
                   ;; Wir fangen den Template-Namen ein
                   (setq captured-template-name name)
                   "DUMMY_TEMPLATE"))
                
                ((symbol-function 'org-grimoire--render-template)
                 (lambda (_tmpl vars _theme)
                   (setq captured-vars vars)
                   "FINAL_HTML_RESULT")))
        
        ;; ==========================================
        ;; Testfall 1: Standard Case
        ;; ==========================================
        (let ((post '(:title "Hello World" :date "2024-01-01" :tags ("org" "lisp") :output "/posts/sample.html")))
          
          (org-grimoire--render-post-item post "/themes/ogt/")
          
          (should (equal captured-template-name "partials/post-item"))
          (should (equal (plist-get captured-vars :title) "Hello World"))
          (should (equal (plist-get captured-vars :url) "/posts/sample.html"))
          (should (equal (plist-get captured-vars :date) "2024-01-01"))
          (should (equal (plist-get captured-vars :tags) "org, lisp")))
        
        ;; ==========================================
        ;; Testfall 2: Edge case (Leer)
        ;; ==========================================
        (let ((post '(:output "/posts/sample.html")))
          (setq captured-vars nil) ;; Reset!
          
          (org-grimoire--render-post-item post "/themes/ogt/")
          
          (should (equal (plist-get captured-vars :title) "Untitled"))
          (should (equal (plist-get captured-vars :date) ""))
          (should (equal (plist-get captured-vars :tags) "")))
        
        ;; ==========================================
        ;; Testfall 3: Edge case (Tags explizit nil)
        ;; ==========================================
        (let ((post '(:title "No Tags" :tags nil :output "/posts/sample.html")))
          (setq captured-vars nil) ;; Reset
          
          (org-grimoire--render-post-item post "/themes/ogt/")
          
          (should (equal (plist-get captured-vars :title) "No Tags"))
          (should (equal (plist-get captured-vars :tags) "")))))))

;; Tags
(ert-deftest ogt-collect-tags ()
  "Test `org-grimoire--collect-tags' builds correct tag-to-post mappings."
  (let* ((post-a '(:title "Post A" :tags ("emacs" "lisp")))
         (post-b '(:title "Post B" :tags ("lisp" "org-mode")))
         (post-c '(:title "Post C" :tags ("emacs")))
         (posts   (list post-a post-b post-c))
         (tags    (org-grimoire--collect-tags posts)))
    (should (hash-table-p tags))
    (should (equal (gethash "emacs" tags) (list post-c post-a)))
    (should (equal (gethash "lisp"  tags) (list post-b post-a)))
    (should (equal (gethash "org-mode" tags) (list post-b)))))

(provide 'org-grimoire-test)
;;; org-grimoire-test.el ends here
