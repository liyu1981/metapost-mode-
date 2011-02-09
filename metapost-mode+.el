;;; metapost-mode+.el --- Drawing with metapost interactively in Emacs

;; Author: Yu LI <liyu1981@gmail.com>
;; Maintainer: Yu LI <liyu1981@gmail.com>
;; Keywords: metapost
;; Version: 0.1
;; Compatibility: GNU Emacs 23.2 ~ newer

;; This file is NOT part of GNU Emacs. 
;; This file is licensed under GPLv3.

;;; Version History

;; v0.1.4 -- Give some notification when epstopdf failed.
;; v0.1.3 -- Detect the latex labels and prompt user to add latex
;;           label support.
;; v0.1.2 -- Error detection and notification after compiling .mp file.
;;         And C-c ` to jump to the error location.
;; v0.1.1 -- metapost with LaTeX now works. With inspriation from Troy
;;         Henderson's web-based Metapost Previewer
;;         (http://www.tlhiv.org/mppreview/)
;; v0.1.0 -- Init version with initial features :)

;;; ToDo List

;; - 

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
(eval-when-compile (require 'doc-view))

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

(defvar metapost-mode+-last-compiliation-failed
  nil
  "The variable indicates whether last compiliation is failed.")

(defvar metapost-mode+-goto-next-error-auto
  t
  "Whether goto next error after a compliation failure.")

(defvar metapost-mode+-who-failed
  "mpost"
  "Which program has failed.")

;;;; metapost-mode+ Keymap

;;;###autoload
(add-hook 'metapost-mode-hook
          (lambda ()
            (define-key meta-mode-map "\C-cl" 'metapost-insert-latex-header)
            (define-key meta-mode-map "\C-cf" 'metapost-insert-figure-environment)
            (define-key meta-mode-map "\C-ct" 'metapost-insert-tex-environment)
            (define-key meta-mode-map "\C-c\C-c" 'metapost-next)
            (define-key meta-mode-map "\C-c`" 'metapost-next-error)))

;;;;

(defun metapost-test ()
  (interactive)
  (if (= 0 (metapost-compile-buffer))
      (message "Succeed!")
    (message "Failed")))

(defun metapost-mode+-strchomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun metapost-insert-latex-header ()
  "Insert the support snippet for LaTeX labels in the begining of
the buffer."
  (interactive)
  (let* ((latex-snippet "prologues:=3;
verbatimtex
%&latex
\\documentclass{minimal}
\\begin{document}
etex\n\n")
         (old-point (+ (point) (length latex-snippet))))
    (goto-char 0)
    (insert-string latex-snippet)
    (goto-char old-point)))

(defun metapost-insert-figure-environment ()
 "Insert a figure environment of metapost."
 (interactive)
 (let* ((metapost-snippet "beginfig()

endfig;\n\n")
        (insert-loc (+ (line-beginning-position) 9)))
   (beginning-of-line)
   (insert-string metapost-snippet)
   (goto-char insert-loc)))

(defun metapost-insert-tex-environment ()
 "Insert a tex environment of metapost."
 (interactive)
 (let* ((metapost-snippet "btex   etex")
        (insert-loc (+ (point) 5)))
   (insert-string metapost-snippet)
   (goto-char insert-loc)))

(defun metapost-detect-latex-mode ()
  (let* ((old-point (point))
         (latex-pattern "%&latex")
         (detected nil))
    (goto-char 0)
    (setq detected (search-forward latex-pattern nil t))
    (goto-char old-point)
    (if detected t nil)))

(defun metapost-detect-latex-usage ()
  (let* ((old-point (point))
         (latex-pattern1 "btex")
         (latex-pattern2 "etex")
         (detected nil))
    (goto-char 0)
    (setq detected (search-forward latex-pattern1 nil t))
    (if (not detected)
        (progn (goto-char 0)
               (setq detected (search-forward latex-pattern2 nil t))))
    (goto-char old-point)
    detected))

(defun metapost-prepare-command (latex-mode)
      metapost-mode+-prog-mpost)

(defun metapost-compile-buffer ()
  "Compile current buffer with metapost."
  (let* ((curbuf-fname (file-name-nondirectory (shell-quote-argument buffer-file-name)))
         (output-buffer (metapost-prepare-buffer curbuf-fname "*metapost: %s*"))
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
  "This function will locate which figure's body your cursor is
in, and return it as the figure no next to be previewed. If your
cursor is out of any figure's body, such as in the
begining/ending of .mp file, it will return the first/last
figure."
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
  "View current figure by first using ``epstopdf'' to convert it
to pdf, and then turn on ``doc-view-mode'' to show it."
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
               (switch-to-buffer-other-window metapost-mode+-current-source-buffer))
      (progn (setq metapost-mode+-who-failed "epstopdf")
             (message
              (format "metapost preview: failed in converting %s. C-c ` to jump to error preview buffer."
                      (file-name-nondirectory curbuf-figure-name)))))))

(defun metapost-next-epstopdf-error ()
  ;;(interactive)
  (let* ((curbuf-fname-full (shell-quote-argument buffer-file-name))
         (preview-buffer
          (format "*metapost-preview: %s*" (file-name-nondirectory curbuf-fname-full)))
         (epstopdf-output-error-line-pattern "^!!! Error:")
         (old-buffer (current-buffer)))
    (switch-to-buffer-other-window preview-buffer)
    (end-of-buffer)
    (re-search-backward epstopdf-output-error-line-pattern)
    (switch-to-buffer-other-window old-buffer)))

(defun metapost-next ()
  "The universal command to compile and preview the editing .mp file.
It will do following things one by one: (1) if it detected that
the editing .mp file is modified, it will ask to save the
file; (2) try to compile it; (3) upon the success of
compiliation, try to convert the figure from eps to pdf and use
doc-view-mode to show it.

The preview figure is smartly deteced by
``metapost-locate-figure-no''."
  (interactive)
  (setq ok-to-preview t)
  (if (metapost-detect-latex-usage)
      (if (metapost-detect-latex-mode)
          t
        (if (y-or-n-p "LaTeX label usage deteced without supporting header, add one?")
            (metapost-insert-latex-header)
          (setq ok-to-preview nil))))
  (if (and ok-to-preview
           (buffer-modified-p))
      (if (y-or-n-p (format "Save %s to preview the figure?" (buffer-file-name)))
          (save-buffer)
        (setq ok-to-preview nil)))
  (if ok-to-preview
      (let* ((old-buffer (current-buffer)))
        (if (= 0 (let* ((metapost-mode+-last-compiliation-failed nil))
                   (metapost-compile-buffer)))
            (progn (metapost-preview)
                   (switch-to-buffer old-buffer))
          (progn 
            (switch-to-buffer old-buffer)
            (setq metapost-mode+-last-compiliation-failed t)

            (if metapost-mode+-goto-next-error-auto
                (progn (message (format "metapost: compile of %s failed."
                                        (file-name-nondirectory buffer-file-name)))
                       (metapost-next-error))
              (progn (setq metapost-mode+-who-failed "mpost")
                     (message (format "metapost: compile of %s failed. C-c ` to jump to error."
                                      (file-name-nondirectory buffer-file-name))))))))))

(defun metapost-next-mpost-error ()
  "This command will try to find the first emergency stop in the
output of last compliation of editing .mp file, extract the error
infomation (i.e., error line no), and move your cursor to the
error location."
  ;;(interactive)
  (if metapost-mode+-last-compiliation-failed
      (let* ((mp-output-buffer
              (concat
               "*metapost: " (file-name-nondirectory
                              (shell-quote-argument buffer-file-name)) "*"))
             (mp-output-error-begin-pattern "^!")
             (mp-output-error-line-pattern "^l\.\\([[:digit:]]+\\)")
             (old-buffer (current-buffer)))
        (switch-to-buffer mp-output-buffer)
        (end-of-buffer)
        (if (re-search-backward mp-output-error-begin-pattern nil t 1)
            (progn (re-search-forward mp-output-error-line-pattern nil t 1)
                   (let* ((error-line-start (match-beginning 1))
                          (error-line-end (match-end 1)))
                     (if (and error-line-start error-line-end)
                         (let* ((line-no
                                 (string-to-int
                                  (metapost-mode+-strchomp
                                   (buffer-substring error-line-start error-line-end)))))
                           (switch-to-buffer old-buffer)
                           (goto-line line-no)
                           (switch-to-buffer-other-window mp-output-buffer)
                           (switch-to-buffer-other-window old-buffer))
                       (message "Oops! Somehow I failed to locate the error line, T_T."))))
          (message "Oops! Seems that there is no error line given, weried *_^. ")))))

(defun metapost-next-error ()
  (interactive)
  (if (string= metapost-mode+-who-failed "mpost")
      (metapost-next-mpost-error)
    (if (string= metapost-mode+-who-failed "epstopdf")
        (metapost-next-epstopdf-error))))

;;;

(provide 'metapost-mode+)

;;; metapost-mode+.el ends here
