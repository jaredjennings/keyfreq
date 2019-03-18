;;; multikeyfreq.el --- track frequencies of chains of commands
;; -*- coding: utf-8 -*-
;;
;; Copyright 2019 by Jared Jennings
;; Copyright 2009-2010, 2015 by David Capello
;; Copyright 2008 by Xah Lee
;; Copyright 2006 by Michal Nazarewicz
;; Copyright 2006 by Ryan Yeske
;;
;; Author: Ryan Yeske, Michal Nazarewicz (mina86/AT/mina86.com)
;; Maintainer: David Capello, Xah lee, Jared Jennings
;; Created: 2006
;;
;; Package-Requires: ((cl-lib "0.5"))
;;
;;
;; Multikeyfreq is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; Version 0.1 - 2019-01 - Jared Jennings
;; * Forked from keyfreq.
;;
;; Version 1.7 - 2015-09 - David Capello
;; * Avoid accumulating key frequency if keyfreq-file-release-lock fails
;;
;; Version 1.6 - 2015-09 - David Capello
;; * Added keyfreq-reset thanks to @w-vi
;; * Fixed issue running multiple instances of Emacs 24.5
;;
;; Version 1.5 - 2014-11 - David Capello
;; * Support cl-lib or cl
;; * Minor doc fixes
;;
;; Version 1.4 - 2010-09 - David Capello
;; * Renamed from command-frequency to keyfreq
;; * Now keyfreq-table holds "deltas"
;; * Removed a lot of "facility" functions to keep the code simple.
;; * Rename keyfreq-summarize-all-modes -> keyfreq-groups-major-modes.
;; * Added keyfreq-filter-major-mode
;; * Added lock file to avoid overwrite the file by two processes at
;;   the same time.
;;
;; Version 1.3 - 2009-09 - David Capello
;; * Added keyfreq-summarize-all-modes
;;
;; Version 1.2 - 2009-09 - David Capello
;; * Now each hash hash-key is a (major-mode . command) cons. Now only
;;   symbols are recorded.
;;
;; Version 1.1 - 2008-09
;; - Replaced the use of this-command var by real-last-command, so
;;   that the commands backward-kill-word, kill-word, kill-line,
;;   kill-region, do not all get counted as kill-region. Changed
;;   post-command-hook to pre-command-hook
;;
;; Version 1.0 - 2007
- Made into a full featured minor mode.  Added full doc
  hash table. Added ability to set user preference using emacs's
;;   customization system. Code is ~400 lines. This version is made by
;;   Michal Nazarewicz in 2007.
;;
;; Version 0.1 - 2006
;; - First version by Ryan Yeske. A quick hack of about 40 lines.
;;

;;; Commentary:
;;
;; HOW TO USE IT?
;;
;; Include the following lines in your .emacs file:
;;
;;   (require 'multikeyfreq)
;;   (multikeyfreq-mode 1)
;;   (multikeyfreq-autosave-mode 1)
;;
;; And use `multikeyfreq-show' to see how many times you used a command.
;;
;;; Code:

(if (not (featurep 'cl-lib))
    (progn
      (require 'cl)
      ;; fix conflict name
      (defalias 'cl-reduce 'reduce))
  (require 'cl-lib))
;; (require 'json)?

(defgroup multikeyfreq nil
  "Customization group for Multikeyfreq mode.
This mode stores number of times each command was called and
provides it as a statistical data."
  :package-version '(multikeyfreq . "0.1")
  :group 'local
  :prefix "multikeyfreq")

;;;###autoload
(define-minor-mode multikeyfreq-mode
  "Multikeyfreq mode records number of times each command was
called making it possible to access usage statistics through
various multikeyfreq-* functions."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'multikeyfreq

  (message (format "multikeyfreq greets you. the mode is %s" multikeyfreq-mode))
  (if multikeyfreq-mode
      (add-hook 'pre-command-hook #'multikeyfreq-pre-command-hook)
    (remove-hook 'pre-ommand-hook #'multikeyfreq-pre-command-hook)))


(defcustom multikeyfreq-buffer "*multifrequencies*"
  "Buffer where frequencies are displayed."
  :group 'multikeyfreq
  :type 'string)


(defcustom multikeyfreq-file "~/.emacs.multikeyfreq"
  "File `multikeyfreq-table' is saved to/loaded from by
`multikeyfreq-table-save' and `multikeyfreq-table-load' functions
by default."
  :group 'multikeyfreq
  :type 'file)

(defcustom multikeyfreq-file-lock "~/.emacs.multikeyfreq.lock"
  "Lock file to update the `multikeyfreq-file'."
  :group 'multikeyfreq
  :type 'file)

(defcustom multikeyfreq-excluded-commands
  '(delete-backward-char ivy-backward-delete-char self-insert-command eshell-send-input)
  "List of commands excluded by multikeyfreq."
  :group 'multikeyfreq
  :type '(symbol))


(defvar multikeyfreq-table (make-hash-table :test 'equal :size 1025)
  "Hash table storing number of times each command was called in each major mode
since the last time the frequencies were saved in `multikeyfreq-file'.")

(defvar multikeyfreq-real-last-last-command '()
  "Antepenultimate command.")

(defun multikeyfreq-pre-command-hook ()
  "Record command execution in `multikeyfreq-table' hash."
  (let ((command real-last-command)
        (antecommand multikeyfreq-real-last-last-command)
        count)
    (when (and command
               (symbolp command))
      (when (and antecommand (symbolp antecommand))
        (setq count (gethash (list major-mode antecommand command)
                             multikeyfreq-table))
        (if (memq command multikeyfreq-excluded-commands)
            (setq multikeyfreq-real-last-last-command '())
          (when (not (eq antecommand command))
            (puthash (list major-mode antecommand command)
                     (if count (1+ count) 1)
                     multikeyfreq-table))))
      (setq multikeyfreq-real-last-last-command command))))

(defun multikeyfreq-groups-major-modes (table)
  "Group major modes in TABLE by command.
Return a hash table where each entry has COMMAND as key and
COUNTER as value."
  (let ((new-table (make-hash-table :test 'equal :size 128)))
    (maphash (lambda (k v)
	       (puthash (cdr k) (+ (gethash (cdr k) new-table 0) v) new-table))
	     table)
    new-table))


(defun multikeyfreq-filter-major-mode (table major-mode)
  "Leave the frequencies of the specified major mode.
Return a hash table where each entry has COMMAND as key and
COUNTER as value."

  (let ((new-table (make-hash-table :test 'equal :size 128)))
    (maphash (lambda (k v)
	       (when (eq (car k) major-mode)
		 (puthash (cdr k) (+ (gethash (cdr k) new-table 0) v) new-table)))
	     table)
    new-table))


(defun multikeyfreq-used-major-modes (table)
  "Return a list with the used major modes (major modes
contained in the TABLE)."
  (let ((list))
    (maphash (lambda (k v)
	       (add-to-list 'list (car k)))
	     table)
    list))


(defun multikeyfreq-list (table &optional reverse limit)
  "Return a cons which car is sum of times any command was used
and cdr is a list of (command . count) pairs.

If REVERSE is nil, sort it starting from the most used command;
if it is `no-sort' the list is not sorted; if it is non-nil and
not `no-sort', sort it from the least used commands.  If LIMIT is
a positive number, only commands which were used more then LIMIT
times will be added.  If it is a negative number, only commands
which were used less then -LIMIT times will be added."

  (let (l (sum 0))
    (maphash
     (cond
      ((or (not (numberp limit)) (= limit 0))
       (lambda (k v) (setq l (cons (cons k v) l) sum (+ sum v))))
      ((= limit -1) (lambda (k v) (setq sum (+ sum v))))
      ((< limit 0)
       (setq limit (- limit))
       (lambda (k v) (setq sum (+ sum v))
	 (if (< v limit) (setq l (cons (cons k v) l)))))
      (t
       (lambda (k v) (setq sum (+ sum v))
	 (if (> v limit) (setq l (cons (cons k v) l))))))
     table)
    (cons sum
	  (cond
	   ((equal reverse 'no-sort) l)
	   (reverse (sort l (lambda (a b) (< (cdr a) (cdr b)))))
	   (t       (sort l (lambda (a b) (> (cdr a) (cdr b)))))))))


(defun multikeyfreq-format-list (list &optional func)
  "Return formatted string with command usage statistics.

The LIST is the `multikeyfreq-table' converted to a list using the `multikeyfreq-list'.

If FUNC is nil each line contains number of times command digram was
called and the command digram; if it is t percentage usage is added in
the middle; if it is 'raw each line will contain number an
command digram separated by single line (with no formatting) otherwise
FUNC must be a function returning a string which will be called
for each entry with four arguments: number of times command digram was
called, percentage usage and the first and second commands of the digram."
  (let* ((sum (car list))
         (max-len
          (cl-reduce (lambda (a b) (max a (cl-reduce (lambda (a b) (max a (length (format "%s" b))))
                                                     (car b) :initial-value 0)))
                     (cdr list)
                     :initial-value 0)))
    (mapconcat
     (cond
      ((not func) (lambda (e) (format "%7d  %s -> %s\n" (cdr e) (caar e) (cdar e))))
      ((equal func t)
       (lambda (e) (format (concat "%7d  %6.2f%%  %-"
                              (format "%d" max-len)
                              "s -> %-"
                              (format "%d" max-len)
                              "s (%s -> %s)\n")
			   (cdr e) (/ (* 1e2 (cdr e)) sum) (caar e) (cadar e)
                           (ignore-errors (multikeyfreq-where-is (caar e)))
                           (ignore-errors (multikeyfreq-where-is (cadar e))))))
      ((equal func 'raw) (lambda (e) (format "%d %s -> %s\n" (cdr e) (caar e) (cdar e))))
      (t (lambda (e) (funcall func (cdr e) (/ (* 1e2 (cdr e)) sum) (caar e) (cdar e)))))
     (cdr list) "")))

(defun multikeyfreq-where-is (command)
  (mapconcat 'key-description
             (where-is-internal command)
             ", "))

(defun multikeyfreq-show (&optional major-mode-symbol)
  "Show command digram usage statistics in `multikeyfreq-buffer'.

If MAJOR-MODE-SYMBOL is given, the function shows the statistics
for that particular major mode only.

With a universal argument, the major-mode of the current buffer
is used as MAJOR-MODE-SYMBOL argument."
  (interactive (list (cond (current-prefix-arg major-mode)
			   (t nil))))

  (let ((table (copy-hash-table multikeyfreq-table)))
    ;; Merge with the values in `multikeyfreq-file'
    (multikeyfreq-table-load table)

    (let* ((list (multikeyfreq-list
		  (cond
		   (major-mode-symbol (multikeyfreq-filter-major-mode table major-mode-symbol))
		   (t (multikeyfreq-groups-major-modes table)))))
	   (formatted-list (multikeyfreq-format-list list t)))

      ;; Display the table
      (display-message-or-buffer (concat (if major-mode-symbol
					     (concat "For " (symbol-name major-mode))
					   (concat "For all major modes"))
					 ":\n\n"
					 formatted-list)
				 multikeyfreq-buffer))))



(defun multikeyfreq-html (filename &optional confirm)
  "Save an HTML file as FILENAME with all the statistics of each mode."

  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write HTML file: "
			     nil nil nil nil)
	   (read-file-name "Write HTML file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))

  (and confirm
       (file-exists-p filename)
       (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
	   (error "Canceled")))

  (let ((table (copy-hash-table multikeyfreq-table))
	(htmltable (lambda (list)
		     (insert "<table>\n")
		     (insert "<thead><tr><th>Times</th><th>Percetage</th><th>Command</th></tr></thead>\n")
		     (insert "<tbody>\n")
		     (multikeyfreq-format-list list
					  (lambda (count perc command1 command2)
					    (insert (format "<tr><td>%d</td><td>%.2f%%</td><td>%s</td><td>%s</td></tr>\n" count perc command1 command2))))
		     (insert "</tbody>\n")
		     (insert "</table>\n"))))

    ;; Merge with the values in `multikeyfreq-file'
    (multikeyfreq-table-load table)

    (with-temp-file filename
      (insert "<html>\n<body>\n")
      (insert "<h1>Multikeyfreq Report</h1>\n")
      (insert "<ul>\n")
      (insert "<li><a href=\"#all\">All major modes</a></li>\n")
      (mapc
       (lambda (major-mode-symbol)
	 (insert (format "<li><a href=\"#%s\">%s</a></li>\n"
			 (symbol-name major-mode-symbol)
			 (symbol-name major-mode-symbol))))
       (multikeyfreq-used-major-modes table))
      (insert "</ul>\n")

      (insert "<h2><a name=\"all\">All major modes</a></h2>\n")
      (funcall htmltable (multikeyfreq-list (multikeyfreq-groups-major-modes table)))

      (mapc
       (lambda (major-mode-symbol)
	 (insert (format "<h2><a name=\"%s\">%s</a></h2>\n"
			 (symbol-name major-mode-symbol)
			 (symbol-name major-mode-symbol)))
	 (funcall htmltable (multikeyfreq-list (multikeyfreq-filter-major-mode table major-mode-symbol))))
       (multikeyfreq-used-major-modes table))

      (insert "</body>\n</html>\n")
      )
    ))


(defun multikeyfreq-reset ()
  "Reset all statistics including those in the file."
  (interactive)
  (when (yes-or-no-p (concat "Delete multikeyfreq file? You will lost all your stats. "))
    ;; clear the hash table
    (clrhash multikeyfreq-table)
    ;; Deal with the file
    (when (multikeyfreq-file-is-unlocked)
      ;; Lock the file
      (multikeyfreq-file-claim-lock)
      ;; Check that we have the lock
      (if (eq (multikeyfreq-file-owner) (emacs-pid))
	  (unwind-protect
	      ;; if the file exists just delete it
	      (if (file-exists-p multikeyfreq-file)
		  (delete-file multikeyfreq-file))
	    ;; Release the lock.
	    (multikeyfreq-file-release-lock))))))


(defun multikeyfreq-file-owner ()
  "Return the PID of the Emacs process that owns the table file lock file."
  (let (owner)
    (and (file-exists-p multikeyfreq-file-lock)
	 (ignore-errors
	   (with-temp-buffer
	     (insert-file-contents-literally multikeyfreq-file-lock)
	     (goto-char (point-min))
	     (setq owner (read (current-buffer)))
	     (integerp owner)))
	 owner)))


(defun multikeyfreq-file-claim-lock ()
  (ignore-errors
    (write-region (number-to-string (emacs-pid)) nil
		  multikeyfreq-file-lock nil 'nomessage nil 'excl)))


(defun multikeyfreq-file-release-lock ()
  (when (file-exists-p multikeyfreq-file-lock)
    (delete-file multikeyfreq-file-lock)))


(defun multikeyfreq-file-is-unlocked ()
  ;; If the lock file exists....
  (if (file-exists-p multikeyfreq-file-lock)
      ;; If the process which has the lock does not exist
      (if (not (memql (multikeyfreq-file-owner) (list-system-processes)))
	  ;; Delete the lock
	  (delete-file multikeyfreq-file-lock)))
  ;; Check again the lock existence (just in case...)
  (not (file-exists-p multikeyfreq-file-lock)))


(defun multikeyfreq-table-save (table &optional mustsave)
  "Append all values from the specified TABLE into the
`multikeyfreq-file' as a sexp of an alist.  Then resets the TABLE
if it was successfully merged.

If MUSTSAVE is t, this function tries to save the table until it
gets the lock and successfully saves it.  If MUSTSAVE is nil, it
does nothing if the table cannot be saved."

  ;; Avoid adding nothing to the file
  (if (> (hash-table-count table) 0)
    (let (done)
      ;; Check that the lock file doesn't exist
      (while (not done)
	(when (multikeyfreq-file-is-unlocked)
	  ;; Lock the file
	  (multikeyfreq-file-claim-lock)

	  ;; Check that we have the lock
	  (if (eq (multikeyfreq-file-owner) (emacs-pid))
	      (unwind-protect
		  (progn
		    ;; Load values and merge them with the current multikeyfreq-table
		    (multikeyfreq-table-load table)

		    ;; Write the new frequencies
		    (with-temp-file multikeyfreq-file
		      (let ((l (cdr (multikeyfreq-list table 'no-sort))))
			(insert "(")
			(dolist (item l)
			  (prin1 item (current-buffer))
			  ;; Easy for git to track if every command is
			  ;; one line
			  (insert "\n"))
			(insert ")"))))

		;; Reset the hash table, enable the 'done' flag, and
		;; release the lock.
		(clrhash table)
		(setq done t)
		(multikeyfreq-file-release-lock))))

	(if (and (not done) mustsave)
	    ;; If we must save the file right now, we'll just keep
	    ;; trying until we can get the lock.  So we can sleep some
	    ;; milliseconds for the next while-loop cycle.
	    (sleep-for 0.1)
	  ;; If we can wait to the next timer's timeout, just enable
	  ;; the 'done' flag to break the while-loop.
	  (setq done t))

	))))


(defun multikeyfreq-table-load (table)
  "Load all values from the `multikeyfreq-file' and add them in the TABLE.
The table is not reset, so the values are appended to the table."

  ;; Does `multikeyfreq-file' exist?
  (if (file-exists-p multikeyfreq-file)
      ;; Load sexp
      (let ((l (with-temp-buffer
		 (insert-file-contents multikeyfreq-file)
		 (goto-char (point-min))
		 (read (current-buffer)))))

	;; Add the values in the table
	(while (and (listp l) l)
	  (if (listp (car l))
          (unless (memq (cdr (caar l)) multikeyfreq-excluded-commands)
            (puthash (caar l) (+ (gethash (caar l) table 0) (cdar l)) table)))
	  (setq l (cdr l)))
	)))


;;;###autoload
(define-minor-mode multikeyfreq-autosave-mode
  "Multikeyfreq Autosave mode automatically saves
`multikeyfreq-table' every `multikeyfreq-autosave-timeout' seconds
and when emacs is killed."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'multikeyfreq

  (when multikeyfreq-autosave--timer
    (cancel-timer multikeyfreq-autosave--timer)
    (setq multikeyfreq-autosave--timer nil))

  (if multikeyfreq-autosave-mode
      (progn
	(setq multikeyfreq-autosave--timer
	      (run-at-time t multikeyfreq-autosave-timeout
			   'multikeyfreq-autosave--do))
	(add-hook 'kill-emacs-hook 'multikeyfreq-mustsave--do))
    (multikeyfreq-mustsave--do)
    (remove-hook 'kill-emacs-hook 'multikeyfreq-mustsave--do)))


(defcustom multikeyfreq-autosave-timeout 600
  "How often in seconds `multikeyfreq-table' should be saved
when `multikeyfreq-autosave-mode' is enabled.  Setting this
value will take effect only after (re)enabling
`multikeyfreq-autosave-mode'."
  :group 'multikeyfreq
  :type 'number)


(defvar multikeyfreq-autosave--timer nil)


(defun multikeyfreq-autosave--do ()
  "Function executed periodically to save the `multikeyfreq-table' in `multikeyfreq-file'."
  (multikeyfreq-table-save multikeyfreq-table))


(defun multikeyfreq-mustsave--do ()
  "Function executed when we close Emacs."
  (multikeyfreq-table-save multikeyfreq-table t))


;;;###autoload
(defun multikeyfreq-save-now ()
  "Save multikeyfreq data now."
  (interactive)
  (multikeyfreq-mustsave--do)
  (message "multikeyfreq data saved into %s" multikeyfreq-file))

(provide 'multikeyfreq)

;;; multikeyfreq.el ends here
