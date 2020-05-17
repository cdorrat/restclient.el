;;; restclient-testing.el --- support for testing with restclient
;;
;; Public domain.

;; Author: Cameron Dorrat <cdorrat@gmail.com>
;; Maintainer: Cameron Dorrat <cdorrat@gmail.com>
;; Created: 1 May 2020
;; Keywords: http restclient
;; Package-Requires: ((restclient "0"))

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to restclient.el to add support for testing by allowign you to:
;;   - running all queries in a file
;;   - allowing cusomt assertions on responses
;;   - providing a ui to see the outcome of all requests

;;; Code:
;;
(require 'jq-mode)

(defvar rctest-output-buffer-name "*RestClient run all*"
  "The name fo the output buffer with run all results")

(defvar rctest--failed-client-asertions nil
  "A list of client specified assertiosn that failed on the last request")

(defvar rctest--num-assertions-registered 0
  "The number of client assertions registered for the current request")

(defun rctest--request-props ()
  "Get an alist of all the 'Tag: value' entries on comments preceeding point"
  (save-excursion
    (let ((end (point))
	  props)
      (backward-sentence)
      (while (re-search-forward "^# \\([^:\n]+\\): \\(.+?\\)$" end t)
	(map-put props (intern (downcase (match-string-no-properties 1))) (match-string-no-properties 2)))
      props)))

(defun rctest--all-request-pos ()
  "return a list of buffer positions with restcleint requests"
  (save-excursion
    (goto-char (point-min))
    (let (matches)
      (while (re-search-forward restclient-method-url-regexp (point-max) t)
	(beginning-of-line)
	(push (point) matches)
	(end-of-line))
      (nreverse matches))))

(defun rctest--new--request-data (buffer a-pos)
  (let* ((req-props (rctest--request-props))
	 (req-url (save-excursion
		    (goto-char a-pos)
		    (buffer-substring-no-properties (line-beginning-position) (- (line-beginning-position 2) 1))))
	 request-data)
    (map-put request-data 'buffer buffer)
    (map-put request-data 'pos a-pos)
    (map-put request-data 'name (or (cdr (assoc 'name req-props)) req-url))
    (map-put request-data 'request req-url)
    request-data))

(defun rctest--update-data-on-start (a-request)
  (map-put a-request 'request-headers headers) ;; var bound in restclient-http-do
  (map-put a-request 'request-body url-request-data)  
  (map-put a-request 'status 'sending)
  a-request)

(defun rctest--update-data-on-end (a-request)
  (map-put a-request 'response-body (buffer-string))
  (map-put a-request 'response-status (save-excursion
					(goto-char (point-min))
					(and (re-search-forward "\\b\\([0-9]\\{3\\}\\)\\b" (line-end-position 2) nil)
					     (string-to-number (match-string 1)))))
  a-request)

(defun rctest--add-request-status (request-data)
  (let* ((status (or (cdr (assoc 'response-status request-data)) 0))
	 (request-failed  (or rctest--failed-client-asertions
			      (and (= 0 rctest--num-assertions-registered)
				   (not (<= 200 status 299))))))
    (map-put request-data 'request-failed request-failed)
    (map-put request-data 'num-assertions rctest--num-assertions-registered)
    (map-put request-data 'failed-assertions rctest--failed-client-asertions)
    request-data))

(defun rctest--http-send-current-sync ()
  (save-window-excursion
   (restclient-http-send-current nil t))
   (while restclient-within-call
     (sit-for 0.05)))


(defun rctest--init-output-buffer (num-requests)
  (save-excursion    
    (with-current-buffer (get-buffer-create rctest-output-buffer-name)
      (read-only-mode -1)
      (erase-buffer)
      (goto-char (point-min))
      (insert (format "Restclient run all output [0/%d]\n" num-requests))
      (org-mode))
    (display-buffer rctest-output-buffer-name t)))

(defun rctest--inc-completed-requests ()
  (with-current-buffer rctest-output-buffer-name
    (goto-char (point-min))
    (and (re-search-forward "\\[\\([0-9]+\\)/[0-9]+\\]" (line-end-position 2))
	 (replace-match (number-to-string (+ 1 (string-to-number (match-string-no-properties 1)))) nil nil nil 1))))

(defun rctest--display-result (request-data)
  (cl-flet ((r-data (k) (cdr (assoc k request-data))))
   (with-current-buffer rctest-output-buffer-name
     (goto-char (point-max))
     (insert (format "\n* [%s] %s\n" (if (r-data 'request-failed) "ERROR" "SUCCESS" ) (r-data 'name)))

     (when (r-data 'request-failed)
       (insert "** Failed Assertions\n")
       (dolist (a (r-data 'failed-assertions))
	 (insert " - " (prin1-to-string a) "\n"))
       (insert "\n"))
     
     (insert "** Request\n")
     (insert (r-data 'request) "\n")
     (when-let ((hdrs (r-data 'request-headers)))
       (dolist (h hdrs)
	 (insert (car h) ": " (cdr h) "\n")))
     
     (when-let ((r-body (r-data 'request-body)))       
       (insert "\n" r-body))
     
     (insert "** Response\n" (r-data 'response-body))
     (outline-hide-sublevels 1))))


(defun rctest-run-all ()
  (interactive)
  (let* ((request-positions (rctest--all-request-pos))
	 (num-requests (length request-positions))
	 (num-failed-requests 0)
	 request-data
	 (start-lambda (lambda ()
			 (setq request-data
			       (rctest--update-data-on-start request-data))))
	 (end-lambda (lambda ()
		       (setq request-data (rctest--update-data-on-end request-data)))))    
    (add-hook 'restclient-http-do-hook start-lambda)
    (add-hook 'restclient-response-received-hook end-lambda)
    
    (setq rctest--failed-client-asertions nil)
    (setq rctest--num-assertions-registered 0)

    (rctest--init-output-buffer num-requests)
    (dolist (req-pos request-positions)
      (goto-char req-pos)
      (setq request-data (rctest--new--request-data (current-buffer) req-pos))
      (rctest--http-send-current-sync)
      (setq request-data (rctest--add-request-status request-data))
      (when (cdr (assoc 'request-failed request-data))
	(incf num-failed-requests))
      (rctest--display-result request-data)
      (rctest--inc-completed-requests))
    
    (with-current-buffer rctest-output-buffer-name
      (goto-char (point-min))
      (forward-line)
      (insert (format "\nCompleted %d Failed, %d successful requests\n" num-failed-requests num-requests))
      (read-only-mode 1)
      (restclient-response-mode t))
    
    (display-buffer rctest-output-buffer-name)

    (remove-hook 'restclient-http-do-hook start-lambda)
    (remove-hook 'restclient-response-received-hook end-lambda)

    
    nil))


;; support for client assertions on responses
(defun rctest--assert-result-function (args offset)
  (goto-char offset)
  (lexical-let ((form (read (current-buffer))))
    (incf rctest--num-assertions-registered)
    (lambda ()
      (condition-case nil
	  (when (not (eval form))
	    (setq rctest--failed-client-asertions (push form rctest--failed-client-asertions)))
	(error
	 (setq rctest--failed-client-asertions (push form rctest--failed-client-asertions)))))))



(provide 'restclient-testing)

(eval-after-load 'restclient
  '(progn
     (resetclient-register-result-func
      "assert" #'rctest--assert-result-function
      "Use an elisep expression to test the success of a request. 
return values of nil or erorrs will be treated as failures")     
     (define-key restclient-mode-map  (kbd "C-c C-r") #'rctest-run-all)))
