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
  (let (request-data)
    (map-put rctest-current-test 'buffer buffer)
    (map-put rctest-current-test 'pos a-pos)
    (map-put rctest-current-test 'name
	     (save-excursion
	       (goto-char a-pos)
	       (buffer-substring-no-properties (line-beginning-position) (- (line-beginning-position 2) 1))))))

(defun rctest--update-data-on-start (a-request)
  (map-put a-request 'request-headers (format "%s" url-request-extra-headers))
  (map-put a-request 'request-body (format "%s" url-request-data))  
  (map-put a-request 'status 'sending)
  a-request)

(defun rctest--update-data-on-end (a-request)
  (map-put a-request 'response-body (buffer-string))
  a-request)

(defun rctest--http-send-current-sync ()
   (restclient-http-send-current nil t)
   (while restclient-within-call
     (sit-for 0.05)))

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
    (add-hook 'restclient-response-loaded-hook end-lambda)
    
    (dolist (req-pos request-positions)
      (goto-char req-pos)
      (setq request-data (rctest--new--request-data (current-buffer) req-pos))
      (rctest--http-send-current-sync)
      ;; update output buffer
      )
    
    (remove-hook 'restclient-http-do-hook start-lambda)
    (remove-hook 'restclient-response-loaded-hook end-lambda)
    nil))

;; (defvar mtest)
;; (setq mtest (with-current-buffer "testing"  (rctest-run-all)))
;; (last mtest)
;; (mapcar (lambda (v) (cdr (assoc 'request-headers v))) mtest)

;;(with-current-buffer "testing"  (first (rctest-run-all)))

(provide 'restclient-testing)

;; (eval-after-load 'restclient
;;   '(progn
;;      (resetclient-register-result-func
;;       "jq-set-var" #'restclient-json-var-function
;;       "Set a resetclient variable with the value jq expression, 
;;        takes var & jq expression as args. 
;;        eg. -> jq-set-var :my-token .token")
;;      (define-key restclient-response-mode-map  (kbd "C-c C-j") #'restclient-jq-interactive-result)))
