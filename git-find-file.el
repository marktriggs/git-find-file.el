;;; git-find-file.el --- Incremental search for files in a git repo
;;
;; Description: Modelled on Command-T from TextMate (I think).
;;
;; Author: Mark Triggs <mark@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Using it:

;;      (define-key global-map (kbd "C-x C-g") 'git-find-file)

;; Then 'C-x C-g' from within your project directory.  You'll see a list of the
;; files in your current Git repository, and anything you type will be used as
;; input to narrow the list.  A pattern like:
;;
;;      => srcmarcmaclj
;;
;; Could match files like:
;;
;;      src/marcgrep/sources/marc_file.clj
;;      src/marcgrep/sources/marcxml_file.clj
;;      src/marcgrep/destinations/marcfile.clj
;;
;; That is, the pattern is applied (very!) fuzzily, and any path where the
;; letters of the pattern appear in order will be returned.
;;
;; There's some basic relevance ranking here: exact substring matches sort above
;; fuzzy matches, fuzzy matches against the path's basename are better than
;; matches against the entire path, and matches where the letters appear close
;; to one another are better than when they're spread out.
;;
;; Hitting RET opens the file under point, and C-g will abort the file selection.
;;
;; Finally, C-s "rotates" the list:
;;
;;      /path/a              /path/b              /path/c
;;      /path/b  --[C-s]-->  /path/c  --[C-s]-->  /path/a
;;      /path/c              /path/a              /path/b
;;

;;; Code:

(require 'cl)

(defvar gff-large-project-threshold 5000
  "Projects with more than this many files will delay updating the file list until `gff-input-delay' seconds have elapsed with no input.")
(defvar gff-input-delay 0.2)


(defun gff-start-of-match (pattern input)
  "If the letters of `pattern' appear in order in `input', return
the position in the string of where they start."
  (block nil
    (let ((len (length pattern)))
      (let ((pattern-pos (1- len))
            (input-pos (length input))
            (letter-distance 0))
        (while (>= pattern-pos 0)
          (let ((next (position (elt pattern pattern-pos) input
                                :from-end t
                                :end input-pos)))
            (if (not next)
                (return (cons nil nil))
              (incf letter-distance (- input-pos next))
              (decf pattern-pos)
              (setq input-pos next))))
        (cons input-pos letter-distance)))))


(defun gff-score-fuzzily (pattern input)
  (destructuring-bind (match . letter-distance)
      (gff-start-of-match pattern input)
    (let* ((basename-start (or (position ?/ input :from-end t)
                               match))
           (raw-score (cond ((not match) nil)
                            ((= (1- match) basename-start) 650)
                            ((> match basename-start)
                             (if (string-match "[[:punct:]]" (string (elt input (1- match))))
                                 600
                               500))
                            (t 300))))
      (when raw-score
        ;; Penalise results where the letters are some distance apart
        (- raw-score letter-distance)))))


(defun gff-scorers-for (pattern)
  "Build a list of scorer functions that will match `pattern'"
  (list
   `(lambda (s) (cond ((string= (file-name-nondirectory s) ,pattern) 1500)
                      ((equal (search ,(concat pattern ".") (file-name-nondirectory s))
                              0) 1150)
                      ((equal (search ,pattern (file-name-nondirectory s))
                              0) 1100)
                      ((search ,pattern (file-name-nondirectory s)) 1000)
                      ((search ,pattern s) 750)
                      (t nil)))
   `(lambda (s)
      (gff-score-fuzzily ,pattern s))))


(defun find-file-in-parent-dir (target dir)
  (let ((dir (expand-file-name dir)))
    (when (not (string= "/" dir))
      (let ((filename (format "%s%s"
                              (file-name-as-directory dir)
                              target)))
        (if (file-exists-p filename)
            filename
          (find-file-in-parent-dir target
                                   (file-name-directory
                                    (directory-file-name dir))))))))

(defun git-find-file ()
  "Run an interactive search for all files in this repo."
  (interactive)
  (let* ((default-directory
           (file-truename
            (file-name-directory
             (expand-file-name
              (or (find-file-in-parent-dir ".git" default-directory)
                  (error "No .git directory found!"))))))
         (files (split-string (shell-command-to-string "git ls-files")
                              "\n" t)))
    (gff-init files)))


(defun gff-update-filter (fn)
  "Apply `fn' to the current filter and replace it with the result."
  (setq gff-active-filter (funcall fn gff-active-filter)
        gff-rotation 0)
  (message "=> %s" gff-active-filter)
  (gff-enqueue-refresh))


(defun gff-enqueue-refresh ()
  "Refresh the buffer once user input seems to have stopped"
  (when gff-keypress-timer
    (cancel-timer gff-keypress-timer))
  (if (> gff-list-size gff-large-project-threshold)
      (setq gff-keypress-timer (run-at-time gff-input-delay nil
                                            'gff-refresh-buffer))
    (gff-refresh-buffer)))


(defun gff-keypress ()
  "Record a keypress"
  (interactive)
  (gff-update-filter `(lambda (s) (concat s ,(string last-input-char)))))


(defun gff-reset ()
  "Set the current filter string back to empty."
  (interactive)
  (gff-update-filter (lambda (s) "")))


(defun gff-backspace ()
  "Remove the last character from the current filter string."
  (interactive)
  (gff-update-filter (lambda (s)
                       (if (> (length s) 0)
                           (subseq s 0 (1- (length s)))
                         s))))

(defun gff-exit ()
  "Bail out."
  (interactive)
  (kill-buffer nil)
  (delete-window))


(defun gff-rotate-list ()
  "Move the first element of the result list to the end."
  (interactive)
  (incf gff-rotation)
  (gff-refresh-buffer))


(defun gff-select ()
  "Select the file at point"
  (interactive)
  (let ((dir default-directory)
        (selection (buffer-substring (line-beginning-position) (line-end-position))))
    (gff-exit)
    (let ((default-directory dir))
      (find-file selection))))


(defun gff-init (files &optional buffer-name score-fn)
  "Initialise the *git-find-file* buffer to display `files'."
  (let ((buffer-name (or buffer-name "*git-find-file*"))
        (score-fn (or score-fn 'gff-scorers-for)))
    (pop-to-buffer (get-buffer-create buffer-name))

    (set (make-local-variable 'gff-list) (sort* (coerce files 'vector) 'string<))
    (set (make-local-variable 'gff-list-size) (length gff-list))
    (set (make-local-variable 'gff-rotation) 0)
    (set (make-local-variable 'gff-active-filter) "")
    (set (make-local-variable 'gff-score-function) score-fn)

    (set (make-local-variable 'gff-last-filtered-list) gff-list)
    (set (make-local-variable 'gff-last-active-filter) "")

    (set (make-local-variable 'gff-keypress-timer) nil)

    (let ((map (make-sparse-keymap)))
      (loop for i from 32 to 127
            do (define-key map (string i) 'gff-keypress))
      (define-key map (kbd "DEL") 'gff-backspace)
      (define-key map (kbd "RET") 'gff-select)
      (define-key map (kbd "C-g") 'gff-exit)
      (define-key map (kbd "C-u") 'gff-reset)
      (define-key map (kbd "C-s") 'gff-rotate-list)
      (use-local-map map))

    (gff-refresh-buffer)))


(defun gff-filter-list (pattern list)
  "Find matches for `pattern' in `list' and return an ordered result set"
  (let* ((scorers (funcall gff-score-function (downcase pattern)))
         (scored-results (map 'vector (lambda (elt)
                                        (cons (or (some (lambda (scorer)
                                                          (funcall scorer (downcase elt)))
                                                        scorers)
                                                  0)
                                              elt))
                              list))
         (sorted-results (sort*
                          (remove-if-not 'plusp scored-results :key 'car)
                          '> :key 'car)))
    (map 'vector 'cdr sorted-results)))


(defun gff-refresh-buffer ()
  "Refresh the *git-find-file* buffer to show the current filtered list of files."
  (let* ((file-list (gff-filter-list gff-active-filter
                                     (if (equal (search gff-last-active-filter gff-active-filter) 0)
                                         ;; We can refine the last list instead of starting from scratch
                                         gff-last-filtered-list
                                       gff-list)))
         (len (length file-list)))

    (setq gff-last-filtered-list file-list)
    (setq gff-last-active-filter gff-active-filter)

    (erase-buffer)
    (if (zerop len)
        (insert "(no matches)")
      (dotimes (i len)
        (insert (elt file-list (mod (+ gff-rotation i) len))
                "\n")))
    (goto-char (point-min))))


(provide 'git-find-file)
