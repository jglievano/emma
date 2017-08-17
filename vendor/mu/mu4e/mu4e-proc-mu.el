;; mu4e-proc-mu.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2017 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mu-backend for mu4e-proc

;;; Code:
(require 'mu4e-utils)

;; internal vars
(defvar mu4e~proc-buf nil
  "Buffer (string) for data received from the backend.")
(defconst mu4e~proc-name " *mu4e-proc*"
  "Name of the server process, buffer.")
(defvar mu4e~proc-process nil
  "The mu-server process.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~proc-send-command (frm &rest args)
  "Send as command to the mu server process.
Start the process if needed."
  (unless (mu4e~proc-running-p)
    (mu4e~proc-start))
  (let ((cmd (apply 'format frm args)))
    (mu4e-log 'to-server "%s" cmd)
    (process-send-string mu4e~proc-process (concat cmd "\n"))))

(defun mu4e~proc-start ()
  "Start the mu server process."
  (unless (file-executable-p mu4e-mu-binary)
    (mu4e-error (format "`mu4e-mu-binary' (%S) not found" mu4e-mu-binary)))
  (let* ((process-connection-type nil) ;; use a pipe
	  (args '("server"))
	  (args (append args (when mu4e-mu-home
			       (list (concat "--muhome=" mu4e-mu-home))))))
    (setq mu4e~proc-buf "")
    (setq mu4e~proc-process (apply 'start-process
			      mu4e~proc-name mu4e~proc-name
			      mu4e-mu-binary args))
    ;; register a function for (:info ...) sexps
    (unless mu4e~proc-process
      (mu4e-error "Failed to start the mu4e backend"))
    (set-process-query-on-exit-flag mu4e~proc-process nil)
    (set-process-coding-system mu4e~proc-process 'binary 'utf-8-unix)
    (set-process-filter mu4e~proc-process 'mu4e~proc-filter)
    (set-process-sentinel mu4e~proc-process 'mu4e~proc-sentinel)))

(defun mu4e~proc-kill ()
  "Kill the mu server process."
  (let* ((buf (get-buffer mu4e~proc-name))
	  (proc (and (buffer-live-p buf) (get-buffer-process buf))))
    (when proc
      (let ((delete-exited-processes t))
	;; the mu server signal handler will make it quit after 'quit'
	(mu4e~proc-send-command "cmd:quit"))
	;; try sending SIGINT (C-c) to process, so it can exit gracefully
      (ignore-errors
	(signal-process proc 'SIGINT))))
  (setq
    mu4e~proc-process nil
    mu4e~proc-buf nil))

;; error codes are defined in src/mu-util
;;(defconst mu4e-xapian-empty 19 "Error code: xapian is empty/non-existent")

(defun mu4e~proc-sentinel (proc msg)
  "Function that will be called when the mu-server process terminates."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (setq mu4e~proc-process nil)
    (setq mu4e~proc-buf "") ;; clear any half-received sexps
    (cond
      ((eq status 'signal)
	(cond
	  ((eq code 9) (message nil))
	    ;;(message "the mu server process has been stopped"))
	  (t (error (format "mu server process received signal %d" code)))))
      ((eq status 'exit)
	(cond
	  ((eq code 0)
	    (message nil)) ;; don't do anything
	  ((eq code 11)
	    (error "Database is locked by another process"))
	  ((eq code 15)
	    (error "Database needs upgrade; try `mu index --rebuild' from the command line"))
	  ((eq code 19)
	    (error "Database empty; try indexing some messages"))
	  (t (error "mu server process ended with exit code %d" code))))
      (t
	(error "Something bad happened to the mu server process")))))

(defsubst mu4e~docid-msgid-param (docid-or-msgid)
  "Construct a backend parameter based on DOCID-OR-MSGID."
  (format
    (if (stringp docid-or-msgid)
      "msgid:\"%s\""
      "docid:%d")
    docid-or-msgid))

(defun mu4e~proc-find (query threads sortfield sortdir maxnum skip-dups include-related)
  "Start a database query for QUERY.
If THREADS is non-nil, show results in threaded fasion, SORTFIELD
is a symbol describing the field to sort by (or nil); see
`mu4e~headers-sortfield-choices'. If SORT is `descending', sort
Z->A, if it's `ascending', sort A->Z. MAXNUM determines the maximum
number of results to return, or nil for 'unlimited'. If SKIP-DUPS
is non-nil, show only one of duplicate messages (see
`mu4e-headers-skip-duplicates').  If INCLUDE-RELATED is non-nil,
include messages related to the messages matching the search
query (see `mu4e-headers-include-related').

For each result found, a function is called, depending on the
kind of result. The variables `mu4e-error-func' contain the
function that will be called for, resp., a message (header row)
or an error."
  (mu4e~proc-send-command
    (concat
      "cmd:find query:%s threads:%s sortfield:%s reverse:%s maxnum:%d "
      "skip-dups:%s include-related:%s")
    (mu4e~escape query)
    (if threads "true" "false")
    ;; sortfield is e.g. ':subject'; this removes the ':'
    (if (null sortfield) "nil" (substring (symbol-name sortfield) 1))
    ;; TODO: use ascending/descending in backend too (it's clearer than 'reverse'
    (if (eq sortdir 'descending) "true" "false")
    (if maxnum maxnum -1)
    (if skip-dups "true" "false")
    (if include-related "true" "false")))

(defun mu4e~proc-move (docid-or-msgid &optional maildir flags)
  "Move message identified by DOCID-OR-MSGID.
At least one of MAILDIR and FLAGS should be specified. Note, even
if MAILDIR is nil, this is still a move, since a change in flags
still implies a change in message filename.

MAILDIR (), optionally
setting FLAGS (keyword argument :flags).  optionally setting FLAGS
in the process. If MAILDIR is nil, message will be moved within the
same maildir.

MAILDIR must be a maildir, that is, the part _without_ cur/ or new/
or the root-maildir-prefix. E.g. \"/archive\". This directory must
already exist.

The FLAGS parameter can have the following forms:
  1. a list of flags such as '(passed replied seen)
  2. a string containing the one-char versions of the flags, e.g. \"PRS\"
  3. a delta-string specifying the changes with +/- and the one-char flags,
     e.g. \"+S-N\" to set Seen and remove New.

The flags are any of `deleted', `flagged', `new', `passed', `replied' `seen' or
`trashed', or the corresponding \"DFNPRST\" as defined in [1]. See
`mu4e-string-to-flags' and `mu4e-flags-to-string'.
The server reports the results for the operation through
`mu4e-update-func'.

If the variable `mu4e-change-filenames-when-moving' is
non-nil, moving to a different maildir generates new names for
the target files; this helps certain tools (such as mbsync).

The results are reported through either (:update ... )
or (:error ) sexp, which are handled my `mu4e-update-func' and
`mu4e-error-func', respectively."
  (unless (or maildir flags)
    (mu4e-error "At least one of maildir and flags must be specified"))
  (unless (or (not maildir) (file-exists-p (concat mu4e-maildir "/" maildir "/")))
    (mu4e-error "Target dir does not exist"))
  (let* ((idparam (mu4e~docid-msgid-param docid-or-msgid))
	  (flagstr
	    (when flags
	      (concat " flags:"
		(if (stringp flags) flags (mu4e-flags-to-string flags)))))
	  (path
	    (when maildir
	      (format " maildir:%s" (mu4e~escape maildir))))
          (rename
            (if (and maildir mu4e-change-filenames-when-moving) "true" "false")))
    (mu4e~proc-send-command "cmd:move %s %s %s %s"
      idparam (or flagstr "") (or path "")
      (format "newname:%s" rename))))

(defun mu4e~proc-index (path my-addresses cleanup lazy-check)
  "Update the message database for filesystem PATH, which should
point to some maildir directory structure. MY-ADDRESSES is a list
of 'my' email addresses (see `mu4e-user-mail-address-list')."
  (let ((path (mu4e~escape path))
	 (addrs (when my-addresses (mapconcat 'identity my-addresses ","))))
    (if addrs
      (mu4e~proc-send-command "cmd:index path:%s my-addresses:%s cleanup:%s lazy-check:%s"
	path addrs (if cleanup "true" : "false") (if lazy-check "true"))
      (mu4e~proc-send-command "cmd:index path:%s" path))))

(defun mu4e~proc-add (path maildir)
  "Add the message at PATH to the database.
With MAILDIR set to the maildir this message resides in,
e.g. '/drafts'; if this works, we will receive (:info add :path
<path> :docid <docid>) as well as (:update <msg-sexp>)."
  (mu4e~proc-send-command "cmd:add path:%s %s"
    (mu4e~escape path)
    (if maildir
      (format "maildir:%s" (mu4e~escape maildir))
      "")))

(defun mu4e~proc-sent (path maildir)
    "Add the message at PATH to the database.
With MAILDIR set to the maildir this message resides in,
e.g. '/drafts'.

 if this works, we will receive (:info add :path <path> :docid
<docid> :fcc <path>)."
    (mu4e~proc-send-command "cmd:sent path:%s maildir:%s"
      (mu4e~escape path) (mu4e~escape maildir)))


(defun mu4e~proc-compose (type decrypt &optional docid)
  "Start composing a message of certain TYPE (a symbol, either
`forward', `reply', `edit', `resend' or `new', based on an
original message (ie, replying to, forwarding, editing,
resending) with DOCID or nil for type `new'.

The result will be delivered to the function registered as
`mu4e-compose-func'."
  (unless (member type '(forward reply edit resend new))
    (mu4e-error "Unsupported compose-type %S" type))
  (unless (eq (null docid) (eq type 'new))
    (mu4e-error "`new' implies docid not-nil, and vice-versa"))
  (mu4e~proc-send-command
    "cmd:compose type:%s docid:%d extract-encrypted:%s use-agent:true"
    (symbol-name type) docid (if decrypt "true" "false")))

(defun mu4e~proc-mkdir (path)
  "Create a new maildir-directory at filesystem PATH."
  (mu4e~proc-send-command "cmd:mkdir path:%s"  (mu4e~escape path)))

(defun mu4e~proc-extract (action docid partidx decrypt &optional path what param)
  "Extract an attachment with index PARTIDX from message with DOCID
and perform ACTION on it (as symbol, either `save', `open', `temp') which
mean:
  * save: save the part to PARAM1 (a path) (non-optional for save)$
  * open: open the part with the default application registered for doing so
  * temp: save to a temporary file, then respond with
             (:temp <path> :what <what> :param <param>)."
  (let ((cmd
	  (concat "cmd:extract "
	    (case action
	      (save
		(format "action:save docid:%d index:%d path:%s extract-encrypted:%s use-agent:true"
		  docid partidx (mu4e~escape path) (if decrypt "true" "false")))
	      (open (format "action:open docid:%d index:%d extract-encrypted:%s use-agent:true"
		  docid partidx (if decrypt "true" "false")))
	      (temp
		(format "action:temp docid:%d index:%d what:%s%s extract-encrypted:%s use-agent:true"
		  docid partidx what
		  (if param
		    (if (stringp param)
		      (format " param:%s" (mu4e~escape param))
		      (format " param:%S" param)) "") (if decrypt "true" "false")))
	      (otherwise (mu4e-error "Unsupported action %S" action))))
	  ))
    (mu4e~proc-send-command "%s" cmd)))


(defun mu4e~proc-ping ()
  "Sends a ping to the mu server, expecting a (:pong ...) in response."
  (mu4e~proc-send-command "cmd:ping"))

(defun mu4e~proc-contacts (personal after)
  "Sends the contacts command to the mu server.
A (:contacts (<list>)) is expected in response. If PERSONAL is
non-nil, only get personal contacts, if AFTER is non-nil, get
only contacts seen AFTER (the time_t value)."
  (mu4e~proc-send-command
    "cmd:contacts personal:%s after:%d"
    (if personal "true" "false")
    (or after 0)))

(defun mu4e~proc-view (docid-or-msgid &optional images decrypt)
  "Get one particular message based on its DOCID-OR-MSGID.
Optionally, if IMAGES is non-nil, backend will any images
attached to the message, and return them as temp files.
The result will be delivered to the function registered as
`mu4e-view-func'."
  (mu4e~proc-send-command
    "cmd:view %s extract-images:%s extract-encrypted:%s use-agent:true"
    (mu4e~docid-msgid-param docid-or-msgid)
    (if images "true" "false")
    (if decrypt "true" "false")))

(defun mu4e~proc-view-path (path &optional images decrypt)
  "View message at PATH (keyword argument).
Optionally, if IMAGES is non-nil, backend will any images
attached to the message, and return them as temp files. The
result will be delivered to the function registered as
`mu4e-view-func'."
  (mu4e~proc-send-command
    "cmd:view path:%s extract-images:%s extract-encrypted:%s use-agent:true"
    (mu4e~escape path)
    (if images "true" "false")
    (if decrypt "true" "false")))

(defun mu4e~proc-remove (docid)
  "Remove message identified by docid.
The results are reporter through either (:update ... ) or (:error)
sexp, which are handled my `mu4e-error-func', respectively."
  (mu4e~proc-send-command "cmd:remove docid:%d" docid))


(provide 'mu4e-proc-mu)
;; End of mu4e-proc-mu.el
