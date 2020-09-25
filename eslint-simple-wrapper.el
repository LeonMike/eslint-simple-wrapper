;;; eslint-simple-wrapper.el --- helper for eslinter because problems with flycheck
;;; Commentary:

;;; Code:
(require 'json)

(setq-default originalBuffer (current-buffer))

(defun eslint-simple-wrapper-draw-table (messages)
  "Draw a table to display all messages.  \
MESSAGES List of messages to display into the table."
  (with-current-buffer "*eslint-simple-wrapper-errors-list*"
    (erase-buffer)
    (org-mode)
    (let (
	  (value             (car messages))
	  (remainingElements (cdr messages))
	  (message)
	  (line)
	  (column)
	  )
      (insert "| message | position |\n")
      (insert "|-\n")
      (while (progn
	       (setq message   (gethash "message"   value))
	       (setq line      (gethash "line"      value))
	       (setq column    (gethash "column"    value))
					;(insert message " | " ,line ":" ,column )
	       (insert (concat "| " message " | " (number-to-string line) ":" (number-to-string column) " |"))
	       (message "now writing to error-list: %s | %d:%d" message line column)
	       (setq value             (car remainingElements))
	       (setq remainingElements (cdr remainingElements))
	       (not (null value))
	       )
	)
      )
    )
  )

(defun eslint-simple-wrapper-foreach-message (messages)
  "For each message element in hash table.  \
MESSAGES hola."
  (let (
	(value (car messages))
	(message)
	(line)
	(column)
	(nodeType)
	(endLine)
	(endColumn)
	(linesDiff)
	(startingPos)
	(endingPos)
	(remainingElements (cdr messages))
	(underlineStyle)
	)
    (while (progn
	     (setq message   (gethash "message"   value))
	     (setq line      (gethash "line"      value))
	     (setq column    (gethash "column"    value))
	     (setq endLine   (gethash "endLine"   value))
	     (setq endColumn (gethash "endColumn" value))
	     (message "--------- %s @%d:%d--%d:%d"
		      message
		      line
		      column
		      endLine
		      endColumn)
	     (setq linesDiff (- endLine line))
	     (set-buffer originalBuffer)
	     (goto-char 0)
	     (beginning-of-line line)
	     (forward-char (- column 1))
	     (setq startingPos (point))
	     (goto-char 0)
	     (beginning-of-line endLine)
	     (forward-char (- endColumn 1))
	     (setq endingPos (point))
	     (setq underlineStyle '(:underline (:color "red" :style wave)))
	     (put-text-property startingPos endingPos 'font-lock-face underlineStyle)
	     ;(setq underlineStyle '(:underline (:color "red" :style wave) :help-echo "this should be a tooltip"))
	     (put-text-property startingPos endingPos 'help-echo message)
	     (setq value             (car remainingElements))
	     (setq remainingElements (cdr remainingElements))
	     (not (null value))
	     )
      )
    )
  )

(defun eslint-simple-wrapper-foreach-file (filesHashTable)
  "For each element in hash table."
  (let* (
         (filepath   (gethash "filePath"   filesHashTable "-"))
         (messages   (gethash "messages"   filesHashTable "-"))
         (errorCount (gethash "errorCount" filesHashTable "-"))
         ;;(warningCount        (gethash "warningCount"        (car json) "-"))
         ;;(fixableErrorCount   (gethash "fixableErrorCount"   (car json) "-"))
         ;;(fixableWarningCount (gethash "fixableWarningCount" (car json) "-"))
         ;;(usedDeprecatedRules (gethash "usedDeprecatedRules" (car json) "-"))
         )
    (message "%s (errorCount: %d)"
	     filepath
	     errorCount)
    (if (not (null messages))
	(progn
	  (eslint-simple-wrapper-foreach-message messages)
	  (eslint-simple-wrapper-draw-table messages)
	  )
      (message "clearing properties")
      (set-buffer originalBuffer)
      (remove-text-properties 1 (buffer-size) '(font-lock-face nil))
      (remove-text-properties 1 (buffer-size) '(help-echo nil))
      )
    )
  )

(defun runItSentinel (process event)
  "Sentinel for runIt function.  \
PROCESS.  \
EVENT."
  (princ
   (format "Process: %s had the event '%s'\n" process event))
  (with-current-buffer "*eslint-simple-wrapper-temp*"
    (let* (
	  (content (buffer-string))
	  )
      (let* (
	     (json-object-type 'hash-table)
	     (json-array-type 'list)
	     (json-key-type 'string)
	     (json (json-read-from-string content))
	     )
	(let (
	      (remainingFiles (cdr json))
	      (file (car json))
	      (counter 0)
	      )
	  (while (progn
		   (eslint-simple-wrapper-foreach-file file)
		   (terpri)
		   (setq file (car remainingFiles))
		   (setq remainingElements (cdr remainingFiles))
		   (not (null file))
		   )
	    )
	  )
	)
      )
    )
  )

(defun my-eslint-check-buffer ()
  "Execute sample command."
  (interactive)
  (if (derived-mode-p 'js-mode)
      (progn
	(setq originalBuffer (current-buffer))
	(if (boundp 'eslint-simple-wrapper-node-modules-dir)
	    (let (
		  (executable (concat eslint-simple-wrapper-node-modules-dir "/.bin/eslint"))
		  )
	      (let (
		    (command (list executable "--format" "json" buffer-file-name))
		    (eslint-simple-wrapper-temp)
		    (eslint-simple-wrapper-errors-list)
		    )
		(princ command)
		(setq eslint-simple-wrapper-temp (get-buffer-create "*eslint-simple-wrapper-temp*"))
		(setq eslint-simple-wrapper-errors-list (get-buffer-create "*eslint-simple-wrapper-errors-list*"))
		(set-buffer "*eslint-simple-wrapper-temp*")
		(erase-buffer)
		(set-buffer "*eslint-simple-wrapper-temp*")
		(make-process :name "RUN_SAMPLE"
			      :buffer eslint-simple-wrapper-temp
					;:command (list executable)
			      :command command
			      :sentinel #'runItSentinel
			      ;; :stderr (get-buffer eslint-simple-wrapper-errors-list
			      )
		)
	      )
	  )
	)
    )
  )

(provide 'eslint-simple-wrapper)

(define-minor-mode eslint-simple-wrapper-mode
  "custom implemmentation for use eslint into emacs"
  :lighter " eslint-simple-wrapper"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-x e") 'my-eslint-check-buffer)
	    map)
  (add-hook 'after-save-hook 'my-eslint-check-buffer)
  )

;;; eslint-simple-wrapper ends here
