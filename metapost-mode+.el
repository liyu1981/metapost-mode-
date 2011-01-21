;;; metapost-mode+.el --- Drawing with metapost interactively in Emacs

;; Author: Yu LI <liyu1981@gmail.com>
;; Maintainer: Yu LI <liyu1981@gmail.com>
;; Keywords: metapost
;; Version: 0.1
;; Compatibility: GNU Emacs 23.2 ~ newer

;; This file is NOT part of GNU Emacs. 
;; This file is licensed under GPLv3.

;;; Version History

;; v0.1 -- Init version with initial features :)

;;; ToDo List

;; - /test/LaTeXLabels.mp not work.

;;; Requirements:

;; metapost-mode.el and doc-view.el, which are part of GNU Emacs 23.2
;; or newer.  You also need `epstopdf', which comes with TexLive.

;;; Commentary:

;;; Configuration:

;; Basically metapost-mode+ should be quite usable with its standard settings, so
;; putting
;;
;;     (require 'metapost-mode+)
;;
;; into your `user-init-file' should be enough.

;;; Code:

;;(require 'metapost-mode)
(require 'doc-view)

;;;; Customization Options

;;;; Internal Variables

(defvar metapost-mode+-prog-mpost
  (executable-find "mpost")
  "The mpost executable path.")

(defvar metapost-mode+-prog-epstopdf
  (executable-find "epstopdf")
  "The epstopdf executable path.")

(defvar metapost-mode+-current-source-buffer
  "The working source buffer.")

;;;; metapost-mode+ Keymap

(add-hook 'metapost-mode-hook
          (lambda ()
            (define-key meta-mode-map "\C-c\C-c" 'metapost-next)))

;;;;

(defun metapost-test ()
  (interactive)
  (message (metapost-detect-latex-mode)))

(defun metapost-mode+-strchomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun metapost-detect-latex-mode ()
  (let* ((old-point (point))
         (latex-pattern "%&latex")
         (detected nil))
    (goto-char 0)
    (setq detected (search-forward latex-pattern nil t))
    (goto-char old-point)
    (if detected t nil)))

(defun metapost-prepare-command (latex-mode)
      metapost-mode+-prog-mpost)

(defun metapost-compile-buffer ()
  "Compile current buffer with metapost."
  (let* ((curbuf-fname (file-name-nondirectory (shell-quote-argument buffer-file-name)))
         (output-buffer (concat "*metapost:" curbuf-fname "*"))
         (latex-mode (metapost-detect-latex-mode))
         (sh-cmd (metapost-prepare-command latex-mode)))
        (call-process sh-cmd nil output-buffer nil curbuf-fname)))

(defun metapost-prepare-buffer (buffer-name &optional buffer-name-format)
  (let* ((buffer-string (if buffer-name-format
                            (format buffer-name-format buffer-name)
                             buffer-name))
         (old-buffer (get-buffer buffer-string)))
    (if old-buffer
        (kill-buffer old-buffer))
    (get-buffer-create buffer-string)))

(defun metapost-locate-figure-no ()
  ;;(interactive)
  (let* ((beginfig-pattern "beginfig\\([ \t]*\\)(\\(.*\\))")
         (old-point (point)))
    ;; first we assume already inside some figure's body
    (re-search-backward beginfig-pattern nil t 1)
    (let* ((figure-no-start (match-beginning 2))
           (figure-no-end (match-end 2)))
      (if (not (and figure-no-start figure-no-end))
          ;; now try forward, may be at the begining of file
          (progn (re-search-forward beginfig-pattern nil t 1)
                 (setq figure-no-start (match-beginning 2))
                 (setq figure-no-end (match-end 2))))
      (if (and figure-no-start figure-no-end)
          (progn (goto-char old-point)
                 (metapost-mode+-strchomp
                  (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))))

(defun metapost-preview ()
  "View current figure."
  ;; (interactive)
  (let* ((prog-epstopdf metapost-mode+-prog-epstopdf)
         (curbuf-fname-full (shell-quote-argument buffer-file-name))
         (curbuf-fname-nodirext
          (file-name-sans-extension (file-name-nondirectory curbuf-fname-full)))
         (curbuf-dir (file-name-directory curbuf-fname-full))
         (curbuf-figure-name (concat curbuf-fname-nodirext "." (metapost-locate-figure-no)))
         (preview-buffer
          (metapost-prepare-buffer (file-name-nondirectory curbuf-fname-full)
                                   "*metapost-preview: %s*"))
         (preview-error-buffer
          (metapost-prepare-buffer (file-name-nondirectory curbuf-fname-full)
                                   "*metapost-preview-error: %s*")))
    (if (= 0 (let* ((coding-system-for-read 'raw-text)
                    (coding-system-for-write 'raw-text))
               (call-process prog-epstopdf
                             curbuf-figure-name
                             preview-buffer
                             preview-error-buffer
                             ;; args
                             "-f")))
        (progn (setq metapost-mode+-current-source-buffer (current-buffer)) 
               (switch-to-buffer-other-window preview-buffer)
               (set-buffer-file-coding-system 'no-conversion)
               (set-buffer-modified-p nil)
               (toggle-read-only)
               (doc-view-mode)
               (switch-to-buffer-other-window metapost-mode+-current-source-buffer)))))

(defun metapost-next ()
  (interactive)
  (setq ok-to-preview t)
  (if (buffer-modified-p)
      (if (y-or-n-p (format "Save %s to preview the figure?" (buffer-file-name)))
          (save-buffer)
        (setq ok-to-preview nil)))
  (if ok-to-preview
      (if (metapost-compile-buffer)
          (metapost-preview)
        (message (concat "metapost compile of " curbuf-fname " FAILED.")))))

;;; metapost-mode+.el ends here
