
(defpackage cliaspora
  (:use :cl
        :quri
        :str
        :cl-json))

(in-package :cliaspora)


(defvar *pod* ""
  "pod url")

(defvar *username* nil
  "username for the pod.")

(defvar *password* nil)

(defvar login-path "/users/sign_in")

(defvar *csrf-token* nil
  "A token needed before login (and at each connection).
   It is return in a meta html tag.")

(defvar *session-cookie* nil
  "Cookies to use for each request.")


(defun get-csrf-token (&key cookie)
  ;; key the csrf token before login.
  (let* ((resp (dex:get (str:concat *pod* "/stream")
                        :cookie-jar cookie))
         (scan (multiple-value-bind (all token)
                    (cl-ppcre:scan-to-strings "csrf-token.*content.*\"\(.*\)\"" resp)
                 (list all token))))

    (setf *csrf-token* (aref (second scan) 0))))

(defun assoc-value (key alist)
  (cdr (assoc key alist)))

(defun login (&key (username *username*) (password *password*) (pod *pod*))
  ;; exple: https://github.com/fukamachi/dexador/issues/4
  (when (str:blank? pod)
    (error "The pod url is not defined."))
  (assert username)
  (assert password)
  (let* ((cookie (cl-cookie:make-cookie-jar))
         (header '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:38.0) Gecko/20100101 Firefox/38.0")))
         (csrf-token (get-csrf-token :cookie cookie))
         (params `(("user[username]" . ,username)
                   ("user[password]" . ,password)
                   ("user[remember_me]" . 1)
                   ("authenticity_token" . ,csrf-token))))

    (print cookie)
    (format t "~%params: ~a~&" params)

    (multiple-value-bind (a b c d)
        (dex:post (str:concat pod login-path)
                  :cookie-jar cookie
                  :headers header
                  :content params
                  :verbose t
                  :keep-alive t)
      (list a b c d))

    (print cookie)
    (setf *session-cookie* cookie)))

(defparameter *json-stream* nil
  "json stream")

(defun get-stream ()
  (setf *json-stream*
        (decode-json-from-string
         (dex:get (str:concat *pod* "/stream.json")
                  :cookie-jar *session-cookie*
                  :verbose t))))


(defun text (post)
  (assoc-value :text post))
