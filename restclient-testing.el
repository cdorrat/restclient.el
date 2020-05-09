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
  a-request)

(defun rctest--http-send-current-sync ()
   (restclient-http-send-current nil t)
   (while restclient-within-call
     (sit-for 0.05)))


(defun rctest--init-output-buffer (num-requests)
  (save-excursion    
    (with-current-buffer (get-buffer-create rctest-output-buffer-name)
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
     (insert (format "\n* %s\n" (r-data 'name)))

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
	 request-data
	 (start-lambda (lambda ()
			 (setq request-data
			       (rctest--update-data-on-start request-data))))
	 (end-lambda (lambda ()
		       (setq request-data (rctest--update-data-on-end request-data)))))
    (add-hook 'restclient-http-do-hook start-lambda)
    (add-hook 'restclient-response-received-hook end-lambda)

    (rctest--init-output-buffer (length request-positions))
    (dolist (req-pos request-positions)
      (goto-char req-pos)
      (setq request-data (rctest--new--request-data (current-buffer) req-pos))
      (rctest--http-send-current-sync)
      (rctest--display-result request-data)
      (rctest--inc-completed-requests))
    
    (remove-hook 'restclient-http-do-hook start-lambda)
    (remove-hook 'restclient-response-received-hook end-lambda)
    nil))

;; (defvar mtest)
;; (setq mtest (with-current-buffer "testing"  (rctest-run-all)))
;; (last mtest)
;; (mapcar (lambda (v) (cdr (assoc 'request-headers v))) mtest)

;;(with-current-buffer "testing"  (first (rctest-run-all)))

(provide 'restclient-testing)

(eval-after-load 'restclient
  '(progn
     ;; (resetclient-register-result-func
     ;;  "jq-set-var" #'restclient-json-var-function
     ;;  "Set a resetclient variable with the value jq expression, 
     ;;   takes var & jq expression as args. 
     ;;   eg. -> jq-set-var :my-token .token")
     (define-key restclient-response-mode-map  (kbd "C-c C-r") #'rctest-run-all)))
