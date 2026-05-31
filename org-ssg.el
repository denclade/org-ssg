;;; org-ssg.el --- Emacs-native static site generator -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dennis Burgermeister <dennis@dencla.de>
;; Copyright (C) 2026 Strahinja Piperac <sp@spiperac.dev>
;;
;; Author: Dennis Burgermeister <dennis@dencla.de>
;; Original Author: Strahinja Piperac <sp@spiperac.dev>
;; Version: 0.9.0
;; Package-Requires: ((emacs "30.2") (org "9.7.11"))
;; Keywords: files, hypermedia, outlines, text
;; URL: https://github.com/dencla/org-ssg

;; License: GPL-3.0-or-later

;; This package is based on org-grimoir but changed various defaults,
;; as well as added various new features. Thus the package was renamed
;; to prevent confusion with the original org-grimoire as well as
;; grimoire.org.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; org-ssg is a static site generator for Emacs and Org mode.
;; Configured and run entirely through your Emacs init file.
;;
;; Usage:
;;   (org-ssg-setup "my-site"
;;     :base-dir    "~/my-site"
;;     :base-url    "https://example.com"
;;     :site-title  "My Site"
;;     :description "A personal site"
;;     :theme       "mytheme")
;;
;;   (org-ssg-build "my-site")
;;
;; :site-title is the global name of your site, used in the <title> element,
;; feeds, and navigation.  The per-page :title placeholder in templates is
;; filled with each post's own #+TITLE keyword.
;;
;; A post is represented as a plist:
;;   (:title        "My Post"
;;    :date         "2026-01-15"
;;    :tags         ("emacs" "lisp")
;;    :slug         "my-post"
;;    :source       "/path/to/file.org"
;;    :output       "/path/to/output/my-post.html"
;;    :assets       ("/path/to/images/screenshot.png"))

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'org)
(require 'org-element)
(require 'ox-html)

(declare-function httpd-start "simple-httpd")
(defvar httpd-root)
(defvar httpd-port)

;;; ============================================================================
;;; Internal state:
;;; ============================================================================

(defvar org-ssg--build-version 0
  "Timestamp of the last successfull build (used for live reload).")

(defvar org-ssg--config nil
  "The configuration plist for the currently executing command.
Bound dynamically; do not set this globally.")

(defvar org-ssg-config-file ".ssg.el"
  "Name of the project-local configuration file.")

(defconst org-ssg--core-keywords
  '("TITLE" "DATE" "TYPE" "DRAFT" "LISTED" "TAGS" "FILETAGS" "CSS" "JS" "ASSETS" "DESCRIPTION")
  "Keywords directly supported by org-ssg, achiving some kind of functionality.")

(defvar org-ssg--global-tags-table nil
  "Hash table of tags with count of associated posts.
Used to access global tag counts during post rendering.")

(defvar org-ssg--log nil
  "Accumulated log entries for the current build.
Each entry is a cons cell (LEVEL . MESSAGE) where LEVEL is one of
:info, :warn, or :error.")

(defvar org-ssg--package-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                           buffer-file-name))
  "Directory containing org-ssg.el.")

(defvar org-ssg--watch-descriptors nil
  "List of active file system watchers.")

(defvar org-ssg--watch-timer nil
  "Timer for debouncing of the build events.
Debouncing is required as the OS sometimes fires 2-3 events per change."
  )

;;; ============================================================================
;;; Configuration:
;;; ============================================================================

(defun org-ssg--config-get (key)
  "Return the value of KEY from the dynamically bound configuration.
Throws an error if the configuration is not loaded."
  (unless org-ssg--config
    (error "The org-ssg config is not loaded.  Call this inside a valid context!"))
  (plist-get org-ssg--config key))

(defun org-ssg--load-config ()
  "Locate, read, and resolve the project-local config."
  (let ((root (locate-dominating-file default-directory org-ssg-config-file)))
    (unless root
      (user-error "No %s found in current or parent directories" org-ssg-config-file))
    (let* ((config-file (expand-file-name org-ssg-config-file root))
           (base-dir    (file-name-directory config-file))
           (config      (with-temp-buffer
                          (insert-file-contents config-file)
                          (goto-char (point-min))
                          (read (current-buffer)))))
      (unless (listp config)
        (user-error "Config file %s must contain a valid property list (plist)" config-file))
      (org-ssg--resolve-config (plist-put config :base-dir base-dir)))))

(defun org-ssg--resolve-config (args)
  "Return a resolved configuration plist derived from ARGS.
Expand :base-dir and derive :source, :output, and :static when absent.
Resolve :theme relative to the themes/ subdirectory of :base-dir."
  (let ((base  (plist-get args :base-dir))
        (theme (plist-get args :theme))
        (final args))
    (when base
      (setq base (expand-file-name base))
      (when theme
        (setq final (plist-put final :theme
                               (expand-file-name theme
                                                 (expand-file-name "themes" base)))))
      
      (dolist (key '(:source :output))
        (unless (plist-member final key)
          (setq final
                (plist-put final key
                           (pcase key
                             (:source (expand-file-name "content" base))
                             (:output (expand-file-name "public_html" base)))))))
      
      (let ((has-static (plist-member final :static))
            (static-val (plist-get final :static)))
        (setq final
              (plist-put final :static
                         (if (not has-static)
                             ;; Fallback: no :static cfg
                             (list (expand-file-name "static" base))
                           (cond
                            ;; Single string (one directory)
                            ((stringp static-val)
                             (list (expand-file-name static-val base)))
                            ;; list of strings (0-n directories)
                            ((listp static-val)
                             (mapcar (lambda (dir) (expand-file-name dir base)) static-val))
                            ;; Fallback: nil
                            (t nil)))))))
    (let ((hook (plist-get final :on-collect-post)))
      (when hook
        (setq final (plist-put final :on-collect-post (eval hook t))))
      final)))

(defun org-ssg--validate-config (config)
  "Validate the required fields in CONFIG, signaling a user error if invalid."
  (let ((source     (plist-get config :source))
        (output     (plist-get config :output))
        (base-url   (plist-get config :base-url))
        (site-title (plist-get config :site-title))
        (per-page   (or (plist-get config :per-page) 10)))
    (unless (and source (file-directory-p source))
      (user-error "Invalid or missing :source directory"))
    (unless output
      (user-error "Missing :output directory"))
    (unless (and base-url (string-match-p "\\`https?://" base-url))
      (user-error "Missing or invalid :base-url (must start with http/https)"))
    (unless (and site-title (not (string-empty-p site-title)))
      (user-error "Missing or empty :site-title"))
    (unless (and (integerp per-page) (> per-page 0))
      (user-error ":per-page must be a positive integer"))))

;;; ============================================================================
;;; Build Logger:
;;; ============================================================================

(defun org-ssg--log-reset ()
  "Clear the build log."
  (setq org-ssg--log nil))

(defun org-ssg--log (level msg)
  "Append a log entry with LEVEL and MSG, and echo it immediately.
LEVEL must be :info, :warn, or :error."
  (push (cons level msg) org-ssg--log)
  (message "[%s] %s"
           (pcase level
             (:info  "INFO")
             (:warn  "WARN")
             (:error "ERROR"))
           msg))

(defun org-ssg--log-summary ()
  "Print a summary of warnings and errors from the current build log."
  (let* ((entries  (reverse org-ssg--log))
         (warnings (cl-remove-if-not (lambda (e) (eq (car e) :warn))  entries))
         (errors   (cl-remove-if-not (lambda (e) (eq (car e) :error)) entries)))
    (if (and (null warnings) (null errors))
        (message "Build completed with no warnings or errors.")
      (message "--- Build Summary ---")
      (dolist (e (append warnings errors))
        (message "  [%s] %s"
                 (if (eq (car e) :warn) "WARN " "ERROR")
                 (cdr e)))
      (message "  %d warning(s), %d error(s)."
               (length warnings) (length errors)))))

;;; ============================================================================
;;; File Utilities
;;; ============================================================================

(defun org-ssg--ignored-file-p (filename)
  "Return non-nil if FILENAME should be ignored.
Ignores mainly Emacs generated lock / temporary stuff."
  (or (string-prefix-p ".#" filename)
      (string-prefix-p "#" filename)
      (string-suffix-p "~" filename)))

(defun org-ssg--resolve-css-path (path)
  "Change .scss extension to .css in PATH.  Leave other paths untouched."
  (if (string-match-p "\\.scss\\'" path)
      (concat (file-name-sans-extension path) ".css")
    path))

(defun org-ssg--process-asset (infile outfile)
  "Copy INFILE to OUTFILE.  If INFILE is an SCSS file, compile it to CSS instead.
Automatically creates target directories if they don't exist."
  (make-directory (file-name-directory outfile) t)
  (if (string-match-p "\\.scss\\'" infile)
      (org-ssg--compile-scss infile (org-ssg--resolve-css-path outfile))
    (copy-file infile outfile t)))

(defun org-ssg--compile-scss (infile outfile)
  "Compile INFILE (SCSS) to OUTFILE (CSS) if a compiler is found on the system."
  (let ((compiler (or (executable-find "sass")
                      (executable-find "sassc")
                      (executable-find "node-sass"))))
    (make-directory (file-name-directory outfile) t)
    (if compiler
        (let ((exit-code (call-process compiler nil nil nil infile outfile)))
          (if (= exit-code 0)
              (org-ssg--log :info (format "Compiled SCSS: %s" (file-name-nondirectory outfile)))
            (org-ssg--log :error (format "SCSS compilation failed for %s" infile))))
      (org-ssg--log :warn (format "No SCSS compiler found! Can't compile %s" infile))
      (copy-file infile outfile t))))

(defun org-ssg--copy-static (static-dir output-dir)
  "Copy all files from STATIC-DIR to OUTPUT-DIR recursively."
  (if (and static-dir (file-exists-p static-dir))
      (let ((count 0))
        (dolist (file (directory-files-recursively static-dir ".*"))
          (let* ((filename (file-name-nondirectory file))
                 (relative (file-relative-name file static-dir))
                 (dest     (expand-file-name relative output-dir)))

            ;; ignore temporary files
            (unless (org-ssg--ignored-file-p filename)
              (org-ssg--process-asset file dest)
              (setq count (1+ count)))))
          (org-ssg--log :info
                        (format "Copied %d static file(s) to %s." count output-dir))
        (org-ssg--log :warn (format "Static dir %s does not exist, did not copy any files." static-dir)))))

(defun org-ssg--copy-theme-static (output-dir theme-dir)
  "Copy static files from THEME-DIR into OUTPUT-DIR/static/ if present."
  (let* ((theme        (or theme-dir (org-ssg--default-theme-dir)))
         (theme-static (expand-file-name "static" theme)))
    (when (file-exists-p theme-static)
      (org-ssg--copy-static theme-static (expand-file-name "static" output-dir)))))

(defun org-ssg--copy-assets (assets source-file output-file)
  "Copy ASSETS into the directory of OUTPUT-FILE.
Asset paths are resolved relative to SOURCE-FILE and mirrored in OUTPUT-FILE."
  (let ((source-dir (file-name-directory source-file))
        (output-dir (file-name-directory output-file)))
    (dolist (asset assets)
      (let* ((relative (file-relative-name asset source-dir))
             (dest     (expand-file-name relative output-dir)))
        (org-ssg--process-asset asset dest)))))

(defun org-ssg--path-equal-p(path1 path2)
  "Return non-nil if PATH1 and PATH2 point to the same file / directory."
  (and path1 path2
       ;; use file-name-as-directory to normalize e.g. ../public_html and ../public_html/
       (string-equal (file-name-as-directory (file-truename path1))
                     (file-name-as-directory (file-truename path2)))))

(defun org-ssg--ensure-absolute-url (url base-dir)
  "Return URL as an absolute path prefixed with BASE-DIR if it is relative."
  (if (or (string-prefix-p "/" url)
          (string-match-p "\\`https?://" url))
      url
    (concat base-dir url)))

(defun org-ssg--parse-asset-str (str)
  "Split STR into a cons cell (source . attributes).
e.g. for #+JS: js/app.js defer"
  (let ((parts (split-string str "[ \t]+" t)))
    (cons (car parts) (string-join (cdr parts) " "))))

(defun org-ssg--build-asset-html (items post-dir template &optional path-resolver)
  "Generate HTML tags for a list of ITEMS (src . attrs) using TEMPLATE.
PATH-RESOLVER is an optional function applied to the source path (e.g., for SCSS)."
  (mapconcat (lambda (item)
               (let* ((src (if path-resolver (funcall path-resolver (car item)) (car item)))
                      (attrs (cdr item))
                      (final-src (org-ssg--ensure-absolute-url src post-dir))
                      (attr-str (if (string-empty-p attrs) "" (concat " " attrs))))
                 (format template final-src attr-str)))
             items ""))

(defun org-ssg--extract-custom-keywords (ast)
  "Extract all non-core keywords from AST into a plist."
  (let ((custom nil))
    (org-element-map ast 'keyword
      (lambda (el)
        (let ((key (org-element-property :key el)))
          (unless (member key org-ssg--core-keywords)
            (let ((sym (intern (concat ":" (downcase (replace-regexp-in-string "_" "-" key)))))
                  (val (org-element-property :value el)))
              (unless (plist-member custom sym)
                (setq custom (plist-put custom sym val))))))))
    custom))

;;; ============================================================================
;;; Org Parsing
;;; ============================================================================

(defun org-ssg--extract-keyword (ast keyword)
  "Return the value of KEYWORD from the Org AST."
  (org-element-map ast 'keyword
    (lambda (el)
      (when (string= (org-element-property :key el) keyword)
        (org-element-property :value el)))
    nil t))

(defun org-ssg--extract-keyword-list (ast keyword)
  "Return a list of all values for KEYWORD from the Org AST."
  (org-element-map ast 'keyword
    (lambda (el)
      (when (string= (org-element-property :key el) keyword)
        (org-element-property :value el)))))

(defun org-ssg--get-excerpt-org (ast)
  "Return the content of a #+begin_excerpt ... #+end_excerpt block from the AST.
Returns nil if no such block exists."
  (let ((block (org-element-map ast 'special-block
                 (lambda (el)
                   (when (string= (upcase (org-element-property :type el)) "EXCERPT")
                     el))
                 nil t)))
    (when block
      (org-element-interpret-data (org-element-contents block)))))

(defun org-ssg--reading-time-from-ast (ast &optional wpm)
  "Return an estimated reading-time string computed from AST.
WPM is the words-per-minute rate; it defaults to 200."
  (let* ((text    (org-element-interpret-data ast))
         (clean   (replace-regexp-in-string "^#\\+[A-Z_]+:.*$" "" text))
         (words   (length (split-string clean "\\W+" t)))
         (minutes (max 0 (round (/ (float words) (or wpm 200))))))
    ;; TODO: configurable string
    (format "Lesezeit: %d Min." minutes)))

(defun org-ssg--file-to-slug (filepath)
  "Return a URL slug derived from FILEPATH."
  (file-name-sans-extension (file-name-nondirectory filepath)))

(defun org-ssg--parse-tags (tags-string)
  "Return a list of tags parsed from TAGS-STRING.
Splits by colons (Org-Mode standard), commas and spaces.
Empty tags are removed."
  (when tags-string
    (split-string tags-string "[:, \t]+" t)))

(defun org-ssg--normalize-boolean (str &optional default)
  "Return the boolean interpretation of STR.
Return DEFAULT when STR is nil.
Treat \"t\", \"true\", and \"yes\" as t; anything else as nil."
  (if (null str)
      default
    (if (member (downcase str) '("t" "true" "yes"))
        t
      nil)))

(defun org-ssg--infer-type (filepath source-dir)
  "Return the post type inferred from FILEPATH relative to SOURCE-DIR.
The type is the name of the immediate subdirectory of SOURCE-DIR."
  (let* ((relative (file-relative-name filepath source-dir))
         (parts    (split-string relative "/")))
    (when (> (length parts) 1)
      (car parts))))

;;; ============================================================================
;;; Collection
;;; ============================================================================

(defun org-ssg--resolve-local-assets (filepath assets-list)
  "Return absolute paths for all relative files under FILEPATH in ASSETS-LIST.
ASSETS-LIST contains lists of e.g. CSS, JS or EXTRA assets."
(let ((file-dir (file-name-directory filepath))
        (resolved nil))
    (dolist (path assets-list)
      (when (and (not (string-prefix-p "/" path))
                 (not (string-match-p "\\`https?://" path)))
        (let ((abs (expand-file-name path file-dir)))
          (cond
           ;; 1. case directory
           ((file-directory-p abs)
            (dolist (file (directory-files-recursively abs ".*"))
              ;; ignore temp files
              (unless (org-ssg--ignored-file-p (file-name-nondirectory file))
                (push file resolved))))
           ;; 2. case single file
           ((file-exists-p abs)
            (push abs resolved))
           ;; 3. nothing
           (t
            (org-ssg--log :warn (format "Local asset missing: %s in %s" path filepath)))))))
    (nreverse resolved)))

(defun org-ssg--collect-assets (ast source-file)
  "Return a list of absolute paths for all file: links found in AST.
Paths are resolved relative to SOURCE-FILE and filtered to those that exist."
  (let ((source-dir (file-name-directory source-file)))
    (delq nil
          (org-element-map ast 'link
            (lambda (el)
              (when (string= (org-element-property :type el) "file")
                (let* ((path     (org-element-property :path el))
                       (absolute (expand-file-name path source-dir)))
                  (if (file-exists-p absolute)
                      absolute
                    (org-ssg--log :error (format "Asset missing: '%s' referenced in '%s'!"
                                                 path
                                                 (file-name-nondirectory source-file)))
                    nil))))))))

(defun org-ssg--collect-file (filepath source-dir output-dir)
  "Return a post plist by parsing the Org file at FILEPATH.
SOURCE-DIR and OUTPUT-DIR are used to compute the output path and post type."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let* ((ast      (org-element-parse-buffer))
           (title    (org-ssg--extract-keyword ast "TITLE"))
           (date     (org-ssg--extract-keyword ast "DATE"))
           (file-type (org-ssg--extract-keyword ast "TYPE"))
           (inferred  (org-ssg--infer-type filepath source-dir))
           (aliases   (org-ssg--config-get :type-aliases))
           (raw-type    (or (when file-type (downcase file-type)) inferred))
           (type        (or (cdr (assoc raw-type aliases)) raw-type))
           (description (org-ssg--extract-keyword ast "DESCRIPTION"))
           (excerpt     (or (org-ssg--get-excerpt-org ast)
                            description))
           (draft    (org-ssg--normalize-boolean
                      (org-ssg--extract-keyword ast "DRAFT")))
           (listed   (org-ssg--normalize-boolean
                      (org-ssg--extract-keyword ast "LISTED") t))
           (tags     (org-ssg--parse-tags
                      (or (org-ssg--extract-keyword ast "TAGS")
                          (org-ssg--extract-keyword ast "FILETAGS"))))
           (slug     (org-ssg--file-to-slug filepath))
           (css         (mapcar #'org-ssg--parse-asset-str (org-ssg--extract-keyword-list ast "CSS")))
           (js          (mapcar #'org-ssg--parse-asset-str (org-ssg--extract-keyword-list ast "JS")))
           (extra-assets (org-ssg--extract-keyword-list ast "ASSETS"))
           (file-dir  (file-name-directory filepath))
           (local-res   (org-ssg--resolve-local-assets filepath
                                                       (append (mapcar #'car css)
                                                               (mapcar #'car js)
                                                               extra-assets)))
           (assets    (append (org-ssg--collect-assets ast filepath) local-res))
           
           (relative (file-relative-name filepath source-dir))
           (output   (expand-file-name
                      (concat (file-name-sans-extension relative) ".html")
                      output-dir))
           
           (custom-props (org-ssg--extract-custom-keywords ast))
           (base-post   (append (list :title title :date date :type type :draft draft :listed listed
                                      :tags tags :slug slug :source filepath
                                      :reading-time (when (org-ssg--config-get :reading-time)
                                                      (org-ssg--reading-time-from-ast ast))
                                      :output output :assets assets :css css :js js :excerpt excerpt)
                                custom-props))
           (hook        (org-ssg--config-get :on-collect-post)))
      
      (if hook (funcall hook base-post) base-post))))

(defun org-ssg--collect (source-dir output-dir)
  "Return a list of post plists by scanning SOURCE-DIR recursively.
OUTPUT-DIR is used to compute output paths.  Draft posts and files placed
directly in SOURCE-DIR with no type subdirectory are skipped."
  (delq nil
        (mapcar (lambda (f)
                  (let ((post (org-ssg--collect-file f source-dir output-dir)))
                    (cond
                     ((plist-get post :draft)
                      (org-ssg--log :info (format "Skipping draft: %s" f))
                      nil)
                     ((null (plist-get post :type))
                      (org-ssg--log :warn (format "No type directory, skipping: %s" f))
                      nil)
                     (t post))))
                (directory-files-recursively source-dir "\\.org$"))))

(defun org-ssg--sort-posts-by-date (posts)
  "Return POSTS sorted by date, newest first."
  (sort (copy-sequence posts)
        (lambda (a b)
          (string> (or (plist-get a :date) "")
                   (or (plist-get b :date) "")))))

;;; ============================================================================
;;; Template Engine & Rendering:
;;; ============================================================================

(defun org-ssg--default-theme-dir ()
  "Return the path to the default theme.  Read :default-theme-dir from config, falling back to the built-in default."
  (let ((custom-default (org-ssg--config-get :default-theme-dir)))
    (if custom-default
        (expand-file-name custom-default (org-ssg--config-get :base-dir))
      (expand-file-name "themes/default/" org-ssg--package-dir))))

(defun org-ssg--resolve-theme-file (filename theme-dir)
  "Return the absolute path to FILENAME, searching THEME-DIR.
Then the default theme.  Enables the developer to create child themes,
which do not have to override all theme files.  Files not overwritten
are taken from the default theme.
Return nil when FILENAME is not found in either location."
  (let ((user-path    (when theme-dir (expand-file-name filename theme-dir)))
        (default-path (expand-file-name filename
                                        (org-ssg--default-theme-dir))))
    (cond
     ((and user-path (file-exists-p user-path)) user-path)
     ((file-exists-p default-path) default-path))))

(defun org-ssg--load-template (name theme-dir)
  "Return the contents of template NAME.html, resolved via THEME-DIR.
Fall back to the default theme.  Logs an error and returns the original placeholder if not found."
  (let ((resolved (org-ssg--resolve-theme-file (concat name ".html") theme-dir)))
    (if resolved
        (with-temp-buffer
          (insert-file-contents resolved)
          (buffer-string))
      (org-ssg--log :error (format "Template not found: %s.html" name))
      (format "<!-- Template not found: %s.html -->" name))))

(defun org-ssg--process-includes (template theme-dir)
  "Replace {{include file.html}} directives in TEMPLATE with file contents.
Searches THEME-DIR fist, falls back to default-theme if not found.
Logs an error and inserts an HTML comment if the file is missing.
Returns a cons cell (RESULT . CHANGED-P)."
  (let* ((changed nil)
         (result
          (replace-regexp-in-string
           "{{include \\([^}]+\\)}}"
           (lambda (match)
             (setq changed t)
             (let* ((filename (match-string 1 match))
                    (resolved (org-ssg--resolve-theme-file filename theme-dir)))
               (if resolved
                   (with-temp-buffer
                     (insert-file-contents resolved)
                     (buffer-string))
                 (org-ssg--log :error (format "Include not found: %s" filename))
                 (format "" filename))))
           template)))
    (cons result changed)))

(defun org-ssg--render-template (template vars theme-dir)
  "Process a complete TEMPLATE string in steps: includes, loops, conditions, vars."
  (let* ((step1 (org-ssg--process-includes template theme-dir))
         (step2 (org-ssg--process-loops (car step1) vars theme-dir))
         (step3 (org-ssg--process-ifs (car step2) vars theme-dir))
         (step4 (org-ssg--process-vars (car step3) vars))
         (changed (or (cdr step1) (cdr step2) (cdr step3) (cdr step4))))
    (cons (car step4) changed)))

(defun org-ssg--render-recursive (html vars theme-dir)
  "Recursively resolve template tags in HTML up to max depth.
VARS contains the variables which should be used to replace {{key}}.
THEME-DIR is the current themes dir."
  (let ((changed t) (depth 0) (max-depth 15))
    (while (and changed (< depth max-depth))
      (let ((result (org-ssg--render-template html vars theme-dir)))
        (setq html (car result)
              changed (cdr result)
              depth (1+ depth))))
    html))

(defun org-ssg--inject-live-reload (html)
  "Inject live-reload script into HTML if watcher is active."
  (if (and (boundp 'org-ssg--watch-descriptors) org-ssg--watch-descriptors)
      (let ((script "\n<script>\n(function(){let v=null;setInterval(()=>{fetch('/org-ssg-livereload').then(r=>r.text()).then(t=>{if(!v)v=t;else if(v!==t)location.reload();}).catch(()=>null);},1000)})();\n</script>\n</body>"))
        (if (string-match-p "</body>" html)
            (replace-regexp-in-string "</body>" script html t t)
          (concat html script)))
    html))

(defun org-ssg--wrap-base (content title &optional url extra-vars)
  "Return CONTENT wrapped in the base template.
TITLE and URL are used to fill {{title}} and {{url}}.
EXTRA-VARS is an optional plist of additional custom variables.
All global configuration variables are also available in the template."
  (let* ((theme-dir (org-ssg--config-get :theme))
         (base      (org-ssg--load-template "base" theme-dir))
         (all-vars (append (list :content content
                                 :title title
                                 :url (or url ""))
                           extra-vars
                           ;; fallbacks
                           (list :extra-css ""
                                 :extra-js  "")
                           org-ssg--config))
         (rendered  (org-ssg--render-recursive base all-vars theme-dir)))
    (org-ssg--inject-live-reload rendered)))

(defun org-ssg--post-to-vars (post)
  "Convert a POST plist into a flat vars plist for item template rendering."
  (let* ((title      (or (plist-get post :title) "Untitled"))
         (date       (or (plist-get post :date) ""))
         (tags       (plist-get post :tags))
         (url        (org-ssg--post-site-url post))
         (excerpt    (org-ssg--org-string-to-html (plist-get post :excerpt)))
         (base-vars  (list :title title :url url :date date
                           :tags (if tags (string-join tags ", ") "")
                           :excerpt excerpt)))
    (append base-vars
            (cl-loop for (k v) on post by #'cddr
                     unless (memq k '(:tags :date :excerpt :css :js :assets))
                     nconc (list k (if v (if (stringp v) v (format "%s" v)) ""))))))

(defun org-ssg--process-loops (template vars theme-dir)
  "Process {{#each var}}...{{/each}} blocks in TEMPLATE.
VARS must contain a list of plists under the matched key.
Uses the corresponding theme file from THEME-DIR."
  (let ((changed nil))
    (with-temp-buffer
      (insert template)
      (goto-char (point-min))
      ;; {{#each list_name}} content {{/each}}
      (while (re-search-forward "{{\\#each[ \t]+\\([^}]+\\)}}\n?\\(\\(?:.\\|\n\\)*?\\){{/each}}" nil t)
        (setq changed t)
        (let* ((list-key   (intern (concat ":" (match-string 1))))
               (inner-tmpl (match-string 2))
               (items      (plist-get vars list-key))
               (replacement ""))
          (when (listp items)
            (setq replacement
                  (mapconcat (lambda (item)
                               (let ((item-vars (if (plist-get item :source)
                                                    (org-ssg--post-to-vars item)
                                                  item)))
                                 ;;Render inner template for each entry
                                 (car (org-ssg--render-template inner-tmpl item-vars theme-dir))))
                             items "")))
          (replace-match replacement t t)))
      (cons (buffer-string) changed))))

(defun org-ssg--process-ifs (template vars theme-dir)
  "Process {{#if var}}...{{else}}...{{/if}} blocks in TEMPLATE.
VARS contains the property list to check.
Uses the corresponding theme file from THEME-DIR."
  (let ((changed nil))
    (with-temp-buffer
      (insert template)
      (goto-char (point-min))
      ;;  {{#if var}} ... [{{else}} ...] {{/if}}
      (while (re-search-forward "{{\\#if[ \t]+\\([^}]+\\)}}\n?\\(\\(?:.\\|\n\\)*?\\)\\(?:{{else}}\n?\\(\\(?:.\\|\n\\)*?\\)\\)?{{/if}}" nil t)
        (setq changed t)
        (let* ((var-sym    (intern (concat ":" (match-string 1))))
               (if-block   (match-string 2))
               (else-block (match-string 3))
               (val        (plist-get vars var-sym))
               (truthy     (and val
                                (not (equal val ""))
                                (not (equal (downcase (format "%s" val)) "false"))))
               (replacement (if truthy if-block (or else-block ""))))
          
          (setq replacement (car (org-ssg--render-template replacement vars theme-dir)))
          (replace-match replacement t t)))
      (cons (buffer-string) changed))))

(defun org-ssg--process-vars (template vars)
  "Replace {{var}} or {{var | filter}} from VARS in TEMPLATE."
  (let ((changed nil))
    (with-temp-buffer
      (insert template)
      (goto-char (point-min))
      ;; e.g. {{ title }} or {{ date | date_format }}
      ;; Ignore blocks starting with #or /.
      (while (re-search-forward "{{\\([a-zA-Z0-9_-]+\\)\\(?:[ \t]*|[ \t]*\\([a-zA-Z0-9_-]+\\)\\)?}}" nil t)
        (setq changed t)
        (let* ((var-sym (intern (concat ":" (match-string 1))))
               (filter  (match-string 2))
               (val     (plist-get vars var-sym))
               (val-str (if val (format "%s" val) ""))
               (replacement val-str))
          
          (when (and filter (not (string-empty-p val-str)))
            (pcase filter
              ;; filters
              ("date"     (setq replacement (format-time-string "%d. %B %Y" (org-ssg--parse-date val-str))))
              ("upcase"   (setq replacement (upcase val-str)))
              ("downcase" (setq replacement (downcase val-str)))
              (_          (org-ssg--log :warn (format "Unknown filter: %s" filter)))))
          
          (replace-match replacement t t)))
      (cons (buffer-string) changed))))

;;; ============================================================================
;;; Org Export overrides & HTML generation
;;; ============================================================================

(org-link-set-parameters "urlexternal"
                         :export (lambda (path desc backend _info)
                                   (when (eq backend 'html)
                                     (format "<a href=\"%s\" class=\"with-icon external\">%s</a>"
                                             path (or desc path)))))

(org-link-set-parameters "urltool"
                         :export (lambda (path desc backend _info)
                                   (when (eq backend 'html)
                                     (format "<a href=\"%s\" class=\"with-icon box\">%s</a>"
                                             path (or desc path)))))

(org-link-set-parameters "btn"
                         :export (lambda (path desc backend _info)
                                   (when (eq backend 'html)
                                     (format "<a href=\"%s\" class=\"button primary\">%s</a>"
                                             path (or desc path)))))

(defun org-ssg--export-html-settings ()
  "Standardize HTML export settings for org-ssg."
  (setq-local tab-width 8)
  (org-mode)
  (org-export-as 'html nil nil t
                   '(:with-toc nil
                               :with-title nil
                               :section-numbers nil)))

(defun org-ssg--org-to-html (filepath)
  "Return the HTML body string produced by exporting the Org file at FILEPATH."
  (with-temp-buffer
    ;; enable relative includes e.g. html-file includes in .orgs
    (setq default-directory (file-name-directory filepath))
    (insert-file-contents filepath)
    (org-ssg--export-html-settings)))

(defun org-ssg--org-string-to-html (org-string)
  "Return the HTML string produced by exporting ORG-STRING.
Returns an empty string if ORG-STRING is nil."
  (if (not org-string)
      ""
    (with-temp-buffer
      (insert org-string)
      (org-ssg--export-html-settings))))

(defun org-ssg--post-site-url (post)
  "Return the root-relative URL for POST.
Variable output comes from e.g. `org-ssg--collect-file'
    (output   (expand-file-name ..."
  (concat "/"
          (file-relative-name (plist-get post :output)
                              (org-ssg--config-get :output))))

(defun org-ssg--render-post (post)
  "Return the full HTML string for POST rendered with its type template."
  (let* ((theme-dir (org-ssg--config-get :theme))
         (type      (or (plist-get post :type) "page"))
         (tmpl-name (cond ((org-ssg--resolve-theme-file (concat type "-item.html") theme-dir)
                           (concat type "-item"))
                          (t type)))
         (title     (or (plist-get post :title) ""))
         (template  (org-ssg--load-template type theme-dir))
         (content   (org-ssg--org-to-html (plist-get post :source)))
         (date      (or (plist-get post :date) ""))
         (tags      (plist-get post :tags))
         (url       (org-ssg--post-site-url post))

         (post-dir  (file-name-directory url))
         
         (css-html  (org-ssg--build-asset-html (plist-get post :css) post-dir
                                               "<link rel=\"stylesheet\" href=\"%s\"%s>\n  "
                                               #'org-ssg--resolve-css-path))
         
         (js-html   (org-ssg--build-asset-html (plist-get post :js) post-dir
                                               "<script src=\"%s\"%s></script>\n  "))
         ;; default variables
         (base-vars (list :title title
                          :content content
                          :date (if (string-empty-p date) "" (format-time-string "%d. %B %Y" (org-ssg--parse-date date)))
                          :tags (org-ssg--tags-html tags)
                          :reading-time (or (plist-get post :reading-time) "")
                          :slug (plist-get post :slug)))
         
         ;; add user defined properties
         (all-vars  (append base-vars
                            (cl-loop for (k v) on post by #'cddr
                                     unless (memq k '(:date :tags :reading-time :content :css :js :assets))
                                     nconc (list k (if v (if (stringp v) v (format "%s" v)) "")))))
         
         (inner     (car (org-ssg--render-template template all-vars theme-dir))))
  (org-ssg--wrap-base inner title url
                      (list :extra-css css-html
                            :extra-js  js-html))))

(defun org-ssg--write-post (post)
  "Render POST and write it to its output path."
  (let* ((output (plist-get post :output))
         (html   (org-ssg--render-post post))
         (assets (plist-get post :assets)))
    (make-directory (file-name-directory output) t)
    (write-region html nil output)
    (when assets
      (org-ssg--copy-assets assets (plist-get post :source) output))
    (org-ssg--log :info (format "Rendered: %s" output))))

(defun org-ssg--render-all (posts)
  "Render all POSTS to their output paths."
  (dolist (post posts)
    (condition-case err
        (org-ssg--write-post post)
      (error (org-ssg--log :warn (format "Failed to render %s: %s"
                                         (plist-get post :source)
                                         (error-message-string err)))))))

;;; ============================================================================
;;; Index and Pagination
;;; ============================================================================

(defun org-ssg--render-post-item (post theme-dir)
  "Return the HTML string for POST rendered as a list item using THEME-DIR.
Checks theme-dir for TYPE-item.html, if not available uses post-item."
(let* ((title      (or (plist-get post :title) "Untitled"))
         (date       (or (plist-get post :date) ""))
         (tags       (plist-get post :tags))
         (url        (org-ssg--post-site-url post))
         (type       (or (plist-get post :type) "post"))
         (excerpt    (org-ssg--org-string-to-html (plist-get post :excerpt)))
         
         (partial    (concat "partials/" type "-list-item"))
         (resolved   (org-ssg--resolve-theme-file (concat partial ".html") theme-dir))
         (tmpl-name  (if resolved partial "partials/post-item"))
         
         (base-vars  (list :title title :url url :date date
                           :tags (if tags (string-join tags ", ") "")
                           :excerpt excerpt))
         (all-vars   (append base-vars
                             (cl-loop for (k v) on post by #'cddr
                                      unless (memq k '(:tags :date :excerpt :css :js :assets))
                                      nconc (list k (if v (if (stringp v) v (format "%s" v)) ""))))))
    
    (car (org-ssg--render-template (org-ssg--load-template tmpl-name theme-dir) all-vars theme-dir))))

(defun org-ssg--render-post-list (posts theme-dir)
  "Return an HTML string of concatenated list items for POSTS using THEME-DIR."
  (mapconcat (lambda (p) (org-ssg--render-post-item p theme-dir))
             posts "\n"))

(defun org-ssg--paginate (posts per-page)
  "Return POSTS split into a list of pages of PER-PAGE items each."
  (let (pages current (count 0))
    (dolist (post posts)
      (push post current)
      (setq count (1+ count))
      (when (= count per-page)
        (push (nreverse current) pages)
        (setq current nil count 0)))
    (when current
      (push (nreverse current) pages))
    (nreverse pages)))

(defun org-ssg--pagination-html (current-page total-pages theme-dir)
  "Return pagination HTML for CURRENT-PAGE of TOTAL-PAGES using THEME-DIR."
  (let* ((prev-url  (when (> current-page 1)
                      (if (= current-page 2)
                          "/index.html"
                        (format "/page-%d.html" (1- current-page)))))
         (next-url  (when (< current-page total-pages)
                      (format "/page-%d.html" (1+ current-page))))
         (template  (org-ssg--load-template "partials/pagination" theme-dir))
         (prev-html (if prev-url
                        (format "<a href=\"%s\">&larr; Newer</a>" prev-url)
                      ""))
         (next-html (if next-url
                        (format "<a href=\"%s\">Older &rarr;</a>" next-url)
                      "")))
    (car (org-ssg--render-template template
                                   (list :prev prev-html
                                         :next next-html)
                                   theme-dir))))

(defun org-ssg--write-index-page (posts page-num total-pages output-dir &optional plural-type)
  "Write index page PAGE-NUM of TOTAL-PAGES to OUTPUT-DIR.
POSTS is the list of posts to render.  If PLURAL-TYPE is provided,
it writes to a type-specific subdirectory and uses its template."
  (let* ((theme-dir  (org-ssg--config-get :theme))
         (dir        (if plural-type
                         (expand-file-name plural-type output-dir)
                       output-dir))
         (filename   (if (= page-num 1) "index.html" (format "page-%d.html" page-num)))
         (output     (expand-file-name filename dir))
         (tmpl-name  (if (and plural-type
                              (org-ssg--resolve-theme-file (concat plural-type ".html") theme-dir))
                         plural-type
                       "index"))
         (page-title (if plural-type
                         (capitalize plural-type)
                       (org-ssg--config-get :site-title)))
         
         (template   (org-ssg--load-template tmpl-name theme-dir))
         (inner      (car (org-ssg--render-template template
                                                    (list :site-title  (org-ssg--config-get :site-title)
                                                          :description (org-ssg--config-get :description)
                                                          :posts       posts 
                                                          :pagination  (org-ssg--pagination-html
                                                                        page-num total-pages theme-dir))
                                                    theme-dir)))
         (html       (org-ssg--wrap-base inner page-title)))
    
    (make-directory dir t)
    (write-region html nil output)
    (org-ssg--log :info (format "Rendered %s: %s" 
                                (if plural-type (format "%s index" plural-type) "index") 
                                output))))

(defun org-ssg--generate-index (all-posts output-dir)
  "Generate paginated index pages from listed posts in ALL-POSTS to OUTPUT-DIR."
  (condition-case err
      (let* ((per-page (or (org-ssg--config-get :per-page) 10))
             (posts    (org-ssg--sort-posts-by-date
                        (cl-remove-if-not
                         (lambda (p) (plist-get p :listed)) all-posts)))
             (pages    (org-ssg--paginate posts per-page))
             (total    (length pages)))
        (if (null posts)
            (org-ssg--log :warn "No listed posts found.")
          (cl-loop for page-posts in pages
                   for i from 1
                   do (org-ssg--write-index-page
                       page-posts i total output-dir))))
    (error (org-ssg--log :warn (format "Failed to generate index: %s"
                                       (error-message-string err))))))

(defun org-ssg--generate-type-indices (all-posts output-dir)
  "Generate paginated index pages for each type mapped in :type-aliases.
Generats it for ALL-POSTS and exports to OUTPUT-DIR"
  (condition-case err
      (let ((aliases  (org-ssg--config-get :type-aliases))
            (per-page (or (org-ssg--config-get :per-page) 10)))
        (dolist (alias aliases)
          (let* ((plural-type (car alias))
                 (type        (cdr alias))
                 (type-posts  (org-ssg--sort-posts-by-date
                               (cl-remove-if-not
                                (lambda (p) (and (plist-get p :listed)
                                                 (string= (plist-get p :type) type)))
                                all-posts))))
            (when type-posts
              (let* ((pages (org-ssg--paginate type-posts per-page))
                     (total (length pages)))
                (cl-loop for page-posts in pages
                         for i from 1
                         do (org-ssg--write-index-page page-posts i total output-dir plural-type)))))))
    (error (org-ssg--log :warn (format "Failed to generate type indices: %s"
                                       (error-message-string err))))))

;;; ============================================================================
;;; Tags Handling
;;; ============================================================================

(defun org-ssg--collect-tags (posts)
  "Return a hash table mapping each tag string to its list of posts from POSTS."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (post posts)
      (dolist (tag (plist-get post :tags))
        (puthash tag (cons post (gethash tag tags '())) tags)))
    tags))

(defun org-ssg--get-all-tags (source-dir)
  "Return a list of all existing tags across all posts in SOURCE-DIR."
  (let ((tags (make-hash-table :test 'equal)))
    (when (file-exists-p source-dir)
      (dolist (file (directory-files-recursively source-dir "\\.org$"))
        (unless (org-ssg--ignored-file-p (file-name-nondirectory file))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; use regex instead org AST, as it is faster when iterating through all files
            (while (re-search-forward "^#\\+\\(?:FILE\\)?TAGS:[ \t]*\\(.*\\)" nil t)
              (dolist (t-str (org-ssg--parse-tags (match-string 1)))
                (puthash t-str t tags)))))))
    (hash-table-keys tags)))

(defun org-ssg--tag-to-slug (tag)
  "Return a URL-safe slug for TAG."
  (replace-regexp-in-string "-+" "-"
                            (replace-regexp-in-string "[^a-z0-9]" "-" (downcase tag))))

(defun org-ssg--tags-html (tags)
  "Return an HTML string representing TAGS as a linked ssg-tags div.
Uses `org-ssg--global-tags-table` to inject post counts into the title attribute."
  (if tags
      (concat "<div class=\"ssg-tags\">"
              (mapconcat
               (lambda (tag)
                 (let* ((count (if org-ssg--global-tags-table
                                   (length (gethash tag org-ssg--global-tags-table))
                                 0))
                        (single-p (= count 1))
                        (plural (if single-p "" "s"))
                        (title-attr (format "%d post%s with this tag" count plural))
                        (slug (org-ssg--tag-to-slug tag))
                        (tag-inactive (if single-p "tag-inactive" ""))
                        (color-class (concat "tag-" slug)))
                   (format "<a class=\"ssg-tag %s %s\" href=\"/tags/%s.html\" title=\"%s\">%s</a>"
                           color-class tag-inactive slug title-attr tag)))
               tags " ")
              "</div>")
    ""))

(defun org-ssg--render-tag-item (tag count theme-dir)
  "Return HTML for TAG with post COUNT using tag-item partial from THEME-DIR."
  (car (org-ssg--render-template
        (org-ssg--load-template "partials/tag-item" theme-dir)
        (list :name  tag
              :slug  (org-ssg--tag-to-slug tag)
              :count (number-to-string count))
        theme-dir)))

(defun org-ssg--write-tag-page (tag posts output-dir theme-dir)
  "Write a listing page for TAG with its POSTS to OUTPUT-DIR using THEME-DIR."
  (let* ((slug     (org-ssg--tag-to-slug tag))
         (dir      (expand-file-name "tags" output-dir))
         (output   (expand-file-name (concat slug ".html") dir))
         (sorted   (org-ssg--sort-posts-by-date posts))
         (template (org-ssg--load-template "partials/tag-index" theme-dir))
         (inner    (car (org-ssg--render-template template
                                                  (list :title      (concat "Tag: " tag)
                                                        :posts      (org-ssg--render-post-list
                                                                     sorted theme-dir)
                                                        :pagination "")
                                                  theme-dir)))
         (html     (org-ssg--wrap-base inner (concat "Tag: " tag))))
    (make-directory dir t)
    (write-region html nil output)
    (org-ssg--log :info (format "Rendered tag page: %s" output))))

(defun org-ssg--write-tags-index (tags-table output-dir theme-dir)
  "Write the tags index page from TAGS-TABLE to OUTPUT-DIR using THEME-DIR."
  (let* ((dir         (expand-file-name "tags" output-dir))
         (output      (expand-file-name "index.html" dir))
         (template    (org-ssg--load-template "tags" theme-dir))
         (sorted-tags (sort (hash-table-keys tags-table) #'string<))
         (items       (mapconcat
                       (lambda (tag)
                         (org-ssg--render-tag-item
                          tag (length (gethash tag tags-table)) theme-dir))
                       sorted-tags "\n"))
         (inner       (car (org-ssg--render-template template
                                                     (list :title "Tags"
                                                           :tags  items)
                                                     theme-dir)))
         (html        (org-ssg--wrap-base inner "Tags")))
    (make-directory dir t)
    (write-region html nil output)
    (org-ssg--log :info (format "Rendered tags index: %s" output))))

(defun org-ssg--generate-tags (posts output-dir)
  "Generate all tag pages and the tags index from POSTS to OUTPUT-DIR."
  (condition-case err
      (let ((tags      (or org-ssg--global-tags-table (org-ssg--collect-tags posts)))
            (theme-dir (org-ssg--config-get :theme)))
        (maphash (lambda (tag tag-posts)
                   (condition-case tag-err
                       (org-ssg--write-tag-page
                        tag tag-posts output-dir theme-dir)
                     (error (org-ssg--log
                             :warn
                             (format "Failed to render tag page '%s': %s"
                                     tag (error-message-string tag-err))))))
                 tags)
        (org-ssg--write-tags-index tags output-dir theme-dir)
        (org-ssg--log :info (format "Generated %d tag page(s)."
                                    (hash-table-count tags))))
    (error (org-ssg--log :error (format "Failed to generate tags: %s"
                                        (error-message-string err))))))

;;; ============================================================================
;;; Feeds & Sitemap
;;; ============================================================================

(defun org-ssg--parse-date (date-string)
  "Return an internal time value parsed from DATE-STRING (yyyy-mm-dd format)."
  (when date-string
    (date-to-time (concat date-string " 00:00:00"))))

(defun org-ssg--rss-date (date-string)
  "Return an RFC 822 date string derived from DATE-STRING (yyyy-mm-dd format)."
  (when-let ((time (org-ssg--parse-date date-string)))
    (format-time-string "%a, %d %b %Y %T +0000" time t)))

(defun org-ssg--atom-date (date-string)
  "Return an RFC 3339 date string derived from DATE-STRING (yyyy-mm-dd format)."
  (when-let ((time (org-ssg--parse-date date-string)))
    (format-time-string "%FT%TZ" time t)))

(defun org-ssg--escape-xml (str)
  "Return STR with XML special characters escaped.
Escapes &, <, >, and \" in that order to avoid double-escaping."
  (when str
    (replace-regexp-in-string
     "\"" "&quot;"
     (replace-regexp-in-string
      ">" "&gt;"
      (replace-regexp-in-string
       "<" "&lt;"
       (replace-regexp-in-string "&" "&amp;" str))))))

(defun org-ssg--post-url (post base-url output-dir)
  "Return the full URL for POST given BASE-URL and OUTPUT-DIR."
  (let* ((output   (plist-get post :output))
         (relative (file-relative-name output output-dir)))
    (concat (string-trim-right base-url "/") "/" relative)))

(defun org-ssg--rss-item (post base-url output-dir)
  "Return an RSS 2.0 <item> XML string for POST using BASE-URL and OUTPUT-DIR."
  (let ((title (org-ssg--escape-xml (plist-get post :title)))
        (url   (org-ssg--post-url post base-url output-dir))
        (date  (org-ssg--rss-date (plist-get post :date)))
        (tags  (plist-get post :tags)))
    (concat
     "  <item>\n"
     (format "    <title>%s</title>\n" (or title "Untitled"))
     (format "    <link>%s</link>\n" url)
     (format "    <guid>%s</guid>\n" url)
     (when date (format "    <pubDate>%s</pubDate>\n" date))
     (when tags
       (mapconcat (lambda (tag)
                    (format "    <category>%s</category>\n"
                            (org-ssg--escape-xml tag)))
                  tags ""))
     "  </item>\n")))

(defun org-ssg--generate-rss (posts base-url output-dir site-title site-description)
  "Return an RSS 2.0 feed XML string for POSTS.
BASE-URL and OUTPUT-DIR are used to build item URLs.
SITE-TITLE and SITE-DESCRIPTION supply the channel metadata."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<rss version=\"2.0\">\n"
   "<channel>\n"
   (format "  <title>%s</title>\n" (org-ssg--escape-xml site-title))
   (format "  <link>%s</link>\n" base-url)
   (format "  <description>%s</description>\n"
           (org-ssg--escape-xml site-description))
   (format "  <lastBuildDate>%s</lastBuildDate>\n"
           (format-time-string "%a, %d %b %Y %T +0000" nil t))
   (mapconcat (lambda (p) (org-ssg--rss-item p base-url output-dir))
              posts "")
   "</channel>\n"
   "</rss>\n"))

(defun org-ssg--atom-entry (post base-url output-dir)
  "Return an Atom feed <entry> XML string for POST using BASE-URL and OUTPUT-DIR."
  (let ((title  (org-ssg--escape-xml (plist-get post :title)))
        (url    (org-ssg--post-url post base-url output-dir))
        (date   (org-ssg--atom-date (plist-get post :date)))
        (author (org-ssg--config-get :author)))
    (concat
     "  <entry>\n"
     (format "    <title>%s</title>\n" (or title "Untitled"))
     (format "    <link href=\"%s\"/>\n" url)
     (format "    <id>%s</id>\n" url)
     (when date   (format "    <updated>%s</updated>\n" date))
     (when author (format "    <author><name>%s</name></author>\n"
                          (org-ssg--escape-xml author)))
     "  </entry>\n")))

(defun org-ssg--generate-atom (posts base-url output-dir site-title)
  "Return an Atom feed XML string for POSTS.
BASE-URL and OUTPUT-DIR are used to build entry URLs.
SITE-TITLE supplies the feed title."
  (let ((author (org-ssg--config-get :author)))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
     (format "  <title>%s</title>\n" (org-ssg--escape-xml site-title))
     (format "  <link href=\"%s\"/>\n" base-url)
     (format "  <link rel=\"self\" href=\"%s/atom.xml\"/>\n"
             (string-trim-right base-url "/"))
     (format "  <id>%s/</id>\n" (string-trim-right base-url "/"))
     (format "  <updated>%s</updated>\n"
             (format-time-string "%FT%TZ" nil t))
     (when author (format "  <author><name>%s</name></author>\n"
                          (org-ssg--escape-xml author)))
     (mapconcat (lambda (p) (org-ssg--atom-entry p base-url output-dir))
                posts "")
     "</feed>\n")))

(defun org-ssg--generate-feeds (posts output-dir)
  "Write rss.xml and atom.xml to OUTPUT-DIR generated from POSTS."
  (condition-case err
      (let* ((base-url    (org-ssg--config-get :base-url))
             (site-title  (org-ssg--config-get :site-title))
             (description (org-ssg--config-get :description))
             (feed-posts  (org-ssg--sort-posts-by-date posts))
             (rss-path    (expand-file-name "rss.xml" output-dir))
             (atom-path   (expand-file-name "atom.xml" output-dir)))
        (write-region
         (org-ssg--generate-rss
          feed-posts base-url output-dir site-title description)
         nil rss-path)
        (write-region
         (org-ssg--generate-atom feed-posts base-url output-dir site-title)
         nil atom-path)
        (org-ssg--log :info "Generated feeds: rss.xml, atom.xml"))
    (error (org-ssg--log :error (format "Failed to generate feeds: %s"
                                        (error-message-string err))))))

(defun org-ssg--generate-sitemap (posts output-dir)
  "Write sitemap.xml to OUTPUT-DIR generated from POSTS."
  (condition-case err
      (let* ((base-url (org-ssg--config-get :base-url))
             (output   (expand-file-name "sitemap.xml" output-dir))
             (urls     (mapconcat
                        (lambda (p)
                          (format
                           "  <url>\n    <loc>%s</loc>\n    <lastmod>%s</lastmod>\n  </url>"
                           (org-ssg--post-url p base-url output-dir)
                           (or (plist-get p :date) "")))
                        posts "\n")))
        (write-region
         (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                 "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
                 urls "\n"
                 "</urlset>\n")
         nil output)
        (org-ssg--log :info "Generated sitemap.xml"))
    (error (org-ssg--log :error (format "Failed to generate sitemap: %s"
                                        (error-message-string err))))))



;;; ============================================================================
;;; Build & deploy
;;; ============================================================================

;;;###autoload
(defun org-ssg-build ()
  "Build the site for the project in the current directory."
  (interactive)
  (let ((org-ssg--config (org-ssg--load-config)))
    (org-ssg--log-reset)
    (org-ssg--validate-config org-ssg--config)
    (let ((source    (org-ssg--config-get :source))
          (output    (org-ssg--config-get :output))
          (statics   (org-ssg--config-get :static))
          (theme-dir (org-ssg--config-get :theme)))
      (org-ssg--log :info "Build started")
      (org-ssg--log :info (format "Source: %s" source))
      (org-ssg--log :info (format "Output: %s" output))
      (condition-case err
          (let* ((posts (org-ssg--collect source output))
                 (org-ssg--global-tags-table (org-ssg--collect-tags posts)))
            (org-ssg--log :info (format "Collected %d post(s)." (length posts)))
            
            (dolist (static-dir statics)
              (org-ssg--copy-static static-dir (expand-file-name "static" output)))
            
            (org-ssg--copy-theme-static output theme-dir)
            (org-ssg--render-all posts)
            (org-ssg--generate-index posts output)
            (org-ssg--generate-type-indices posts output)
            (org-ssg--generate-tags posts output)
            (org-ssg--generate-feeds posts output)
            (org-ssg--generate-sitemap posts output)
            (setq org-ssg--build-version (float-time))
            (org-ssg--log :info "Build complete.")
            (org-ssg--log-summary))
        (error
         (org-ssg--log :error (error-message-string err))
         (org-ssg--log-summary))))))

;;;###autoload
(defun org-ssg-clean ()
  "Delete the output directory to remove orphaned files and dev artifacts."
  (interactive)
  (let* ((org-ssg--config (org-ssg--load-config))
         (output (org-ssg--config-get :output)))
    (when (and output (file-exists-p output))
      (delete-directory output t)
      (message "org-ssg: Cleaned output directory %s" output))))

;;;###autoload
(defun org-ssg-publish ()
  "Perform a clean production build of the site.
Stops the watcher (to prevent livereload injection), cleans the output
directory, and builds the site from scratch."
  (interactive)
  (org-ssg-stop-watch)
  (org-ssg-clean)
  (org-ssg-build)
  
  (message "org-ssg: Production build finished! Ready for deployment."))

;;; ============================================================================
;;; Creation Helper
;;; ============================================================================

;;;###autoload
(defun org-ssg-new ()
  "Interactively create a new post for the project in the current directory."
  (interactive)
  (let* ((org-ssg--config (org-ssg--load-config))
         (source (org-ssg--config-get :source))
         (dirs   (cl-remove-if-not #'file-directory-p
                                   (directory-files source t "^[^.]")))
         (types  (mapcar #'file-name-nondirectory dirs))
         (type   (completing-read "Type: " types nil t))
         (title  (read-string "Title: "))
         (tags-list (completing-read-multiple "Tags (comma separated): " existing-tags))
         (date   (format-time-string "%F"))
         (slug   (replace-regexp-in-string "[^a-z0-9]" "-" (downcase title)))
         (dir    (expand-file-name type source))
         (file   (expand-file-name (concat slug ".org") dir)))
    (make-directory dir t)
    (write-region
     (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: %s\n#+DRAFT: t\n\n"
             title date tags)
     nil file)
    (find-file file)))

(defun org-ssg--create-frontmatter (title date tags)
  "Return the formatted frontmatter string, insert TITLE, DATE and TAGS."
  (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: %s\n#+DRAFT: t\n\n"
          title date tags))

;;;###autoload
(defun org-ssg-insert-frontmatter ()
  "Interactively prompt for metadata and insert frontmatter at the top of the current file."
  (interactive)
  (let* ((org-ssg--config (org-ssg--load-config))
         (source (org-ssg--config-get :source))
         (existing-tags (org-ssg--get-all-tags source))
         (title  (read-string "Title: "))
         (tags-list (completing-read-multiple "Tags (comma separated): " existing-tags))
         (tags   (string-join tags-list ", "))
         (date   (format-time-string "%F")))
    (save-excursion
      (goto-char (point-min))
      (insert (org-ssg--create-frontmatter title date tags)))
    (message "org-ssg: Frontmatter inserted.")))

;;; ============================================================================
;;; Development server
;;; ============================================================================

;;;###autoload
(defun org-ssg-serve ()
  "Serve the built site for the project in the current directory and open it in the browser."
  (interactive)
  (if (not (require 'simple-httpd nil t))
      (user-error "Simple-httpd is not installed")
    (let* ((org-ssg--config (org-ssg--load-config))
           (output (org-ssg--config-get :output)))
      (setq httpd-root output)

      ;; Live reload
      (defservlet org-ssg-livereload "text/plain" ()
        (insert (number-to-string org-ssg--build-version)))

      (org-ssg-watch)
      
      (httpd-start)
      (let ((url (format "http://localhost:%d" httpd-port)))
        (message "Serving %s at %s" output url)
        (browse-url url)))))

;;;###autoload
(defun org-ssg-stop-serve (&optional force)
  "Stop serving for the project in the current directory.
Does NOT stop the server if it is running in another directory, unless FORCE is a non nil value."
  (interactive "P")
  (if (and (featurep 'simple-httpd) (httpd-running-p))
      (let* ((org-ssg--config (org-ssg--load-config))
             (output (org-ssg--config-get :output)))
        (if (or force (org-ssg--path-equal-p httpd-root output))
            (progn
              (org-ssg-stop-watch)
              (httpd-stop)
              (message "org-ssg: Stopped server & watcher"))
          (message "org-ssg: Server is running on another project. Please stop it there, or use force!")))
    (message "org-ssg: Server did not run.")))

(defun org-ssg--watch-callback (event)
  "Callback for file change.
EVENT is the list coming from `filenotify'."
  (let* ((file (nth 2 event))
         (filename (file-name-nondirectory file)))
    ;; ignored
    (unless (org-ssg--ignored-file-p filename)
      
      ;; cancle additional timer
      (when org-ssg--watch-timer
        (cancel-timer org-ssg--watch-timer))
      
      ;; Create new one
      (setq org-ssg--watch-timer
            (run-with-timer 0.5 nil
                            (lambda ()
                              (message "[Watcher] Change in %s detected. Rebuild..." filename)
                              (org-ssg-build)))))))

(defun org-ssg--watch-add-dir-tree (dir)
  "Add DIR and all subfolders recursively to watchers."
  (when (and dir (file-exists-p dir))
    ;; Den Hauptordner watchen
    (push (file-notify-add-watch dir '(change attribute-change) #'org-ssg--watch-callback)
          org-ssg--watch-descriptors)
    ;; Alle Unterordner finden und watchen (Das 't' am Ende schließt Ordner ein)
    (dolist (subdir (directory-files-recursively dir "^[^.]" t))
      (when (file-directory-p subdir)
        (push (file-notify-add-watch subdir '(change attribute-change) #'org-ssg--watch-callback)
              org-ssg--watch-descriptors)))))

;;;###autoload
(defun org-ssg-stop-watch ()
  "Stops the automatic rebuild / file watcher and clears up active watchers."
  (interactive)
  (dolist (desc org-ssg--watch-descriptors)
    (ignore-errors (file-notify-rm-watch desc)))
  (setq org-ssg--watch-descriptors nil)
  (when org-ssg--watch-timer
    (cancel-timer org-ssg--watch-timer)
    (setq org-ssg--watch-timer nil))
  (message "org-ssg watcher stopped."))

;;;###autoload
(defun org-ssg-watch ()
  "Start a watcher for the current project.
Watches src, static and theme folder for changes."
  "Startet einen Watcher für das aktuelle Projekt.
Überwacht Source-, Static- und Theme-Ordner auf Änderungen und baut die Seite neu."
  (interactive)
  (let* ((org-ssg--config (org-ssg--load-config))
         (raw-sources (org-ssg--config-get :source))
         (raw-statics (org-ssg--config-get :static))
         (sources     (if (listp raw-sources) raw-sources (list raw-sources)))
         (statics     (if (listp raw-statics) raw-statics (list raw-statics)))
         (theme-dir (org-ssg--config-get :theme)))
    
    ;; Clean old watchers (e.g. if calling this function twice)
    (org-ssg-stop-watch)
    
    ;; src
    (dolist (src sources)
      (org-ssg--watch-add-dir-tree src))
    
    ;; static
    (dolist (stat statics)
      (org-ssg--watch-add-dir-tree stat))
    
    ;; theme
    (when theme-dir
      (org-ssg--watch-add-dir-tree theme-dir))
    
    (message "org-ssg watcher running. Observing %d directories."
             (length org-ssg--watch-descriptors))))

;;;###autoload
(defun org-ssg-init (base-dir base-url site-title)
  "Initialize a new org-ssg site at BASE-DIR with BASE-URL & site-title."
  (interactive
   (list (read-directory-name "Base directory: ")
         (read-string "Base URL (e.g. https://example.com): ")
         (read-string "Site Title: ")))
  (let* ((base        (expand-file-name base-dir))
         (content     (expand-file-name "content" base))
         (posts       (expand-file-name "post" content))
         (pages       (expand-file-name "page" content))
         (static      (expand-file-name "static" base))
         (config-file (expand-file-name org-ssg-config-file base)))
    (dolist (dir (list base content posts pages static))
      (make-directory dir t))
    (write-region
     (format "(:site-title  \"%s\"\n :base-url    \"%s\"\n :description \"A fresh org-ssg site\"\n :theme       \"default\"\n :per-page    10)\n"
             site-title base-url)
     nil config-file)
    (write-region
     (concat "#+TITLE: My First Post\n"
             "#+DATE: " (format-time-string "%F") "\n"
             "#+TAGS: emacs\n"
             "#+DRAFT: false\n\n"
             "Hello, world!\n")
     nil (expand-file-name "my-first-post.org" posts))
    (write-region
     "#+TITLE: About\n#+LISTED: false\n\nThis is the about page.\n"
     nil (expand-file-name "about.org" pages))
    (message "Done! Initialized org-ssg project at %s" base)))

(provide 'org-ssg)
;;; org-ssg.el ends here
