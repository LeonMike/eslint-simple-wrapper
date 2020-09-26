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
    (let (
	  (value             (car messages))
	  (remainingElements (cdr messages))
	  (message)
	  (line)
	  (column)
	  (isNotLastElement)
	  )
      (insert "| *position* | *message* |\n")
      (insert "|-\n")
      (while (progn
	       (setq message   (gethash "message"   value))
	       (setq line      (number-to-string (gethash "line"      value)))
	       (setq column    (number-to-string (gethash "column"    value)))
	       (insert
		(concat
		 "| [[file:" (buffer-file-name originalBuffer) "::" line "]"
		 "[" line ":"  column"]] | "
		 message
		 " |"))
	       (setq value             (car remainingElements))
	       (setq remainingElements (cdr remainingElements))
	       (setq isNotLastElement (not (null value)))
	       (if isNotLastElement
		   (progn
		     (insert "\n")
		     t
		     )
		 nil
		 )
	       )
	)
      (org-mode)
      (setq org-hide-emphasis-markers t)
      ;; (defface eslint-simple-wrapper-list-title-face
      ;; 	'((:foregound "green" :background "yellow"))
      ;; 	"Face for titles in *eslint-simple-wrapper-errors-list*"
      ;; 	:group 'org-faces
      ;; 	)
      ;; (add-to-list 'org-emphasis-alist
      ;; 		   '("!" 'font-lock-face (:foreground "red")))
      
      ;; (defface my-face-org-keystroke
      ;; 	'((t (:inherit shadow 
      ;; 		       :box (:line-width -2 ;neg. in order to keep the same size of lines
      ;; 					 :color "grey75"
      ;; 					 :style pressed-button)))) "Face for keystrokes"
      ;; 					 :group 'org-faces)
      ;; ;(add-to-list 'org-emphasis-alist
      ;; 					;		   '("%" eslint-simple-wrapper-list-title-face))
      ;; (add-to-list 'org-emphasis-alist
      ;; 		   (
      ;; 		    ("%" my-face-org-keystroke)
      ;; 		    ))
      ;; (org-mode)
      (org-cycle)
      (goto-char 0)
      (replace-regexp "|" "  ")
      (goto-char 0)
      (replace-regexp "-\\+-" "---")
      )
    )
  )

(defun eslint-simple-wrapper-foreach-message (messages)
  "For each message element in hash table.  \
MESSAGES List of messages to display into the table."
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
      (set-buffer "*eslint-simple-wrapper-errors-list*")
      (erase-buffer)
      (insert "No errors found! Good Job :D")
      (set-buffer originalBuffer)
      (remove-text-properties 1 (buffer-size) '(font-lock-face nil))
      (remove-text-properties 1 (buffer-size) '(help-echo nil))
      )
    )
  )

(defun eslint-simple-wrapper-sentinel (process event)
  "Sentinel for eslint-simple-wrapper-check-buffer function.  \
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
  (kill-buffer "*eslint-simple-wrapper-temp*")
  )

(defun eslint-simple-wrapper-check-buffer ()
  "Execute eslint to check current buffer."
  (interactive)
  (if (derived-mode-p 'js-mode 'js2-mode)
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
			      :sentinel #'eslint-simple-wrapper-sentinel
			      ;; :stderr (get-buffer eslint-simple-wrapper-errors-list
			      )
		)
	      )
	  (message "please define 'eslint-simple-wrapper-node-modules-dir' variable to run eslint-simple-wrapper")
	  )
	)
    )
  )

(provide 'eslint-simple-wrapper)

(define-minor-mode eslint-simple-wrapper-mode
  "custom implemmentation for use eslint into emacs"
  :lighter " eslint-simple-wrapper"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-x e") 'eslint-simple-wrapper-check-buffer)
	    map)
  (add-hook 'after-save-hook 'eslint-simple-wrapper-check-buffer)
  )

;;; eslint-simple-wrapper ends here
