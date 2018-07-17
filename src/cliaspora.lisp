
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

(defclass author ()
  ((id
    :initarg :id
    :accessor author-id)
   (guid
    :initarg :guid
    :accessor author-guid))
  (name
   :initarg :name
   :accessor author-name)
  (diaspora-id
   :initarg :diaspora-id
   :accessor author-diaspora-id)
  (avatar
   ;; a plist with :small, :medium and :large sizes
   :initarg :avatar
   :accessor author-avatar))

(defclass post ()
  ((id
    :initarg :id
    :accessor post-id)
   (guid
    :initarg :guid
    :accessor post-guid)
   (created-at
    :initarg :created-at
    :accessor post-created-at)
   (public
    :initarg :public
    :accessor public)
   (title
    :initarg :title
    :accessor title)
   (author
    :initarg :author
    :accessor author)
   (text
    ;; accessor: generic.
    :initarg :text)
   (likes
    :initarg :likes
    :accessor likes)
   ;; reshares
   (post-type
    :initarg :post-type
    :accessor post-type)
   (nsfw
    :initarg :nsfw
    :accessor nsfw)
   ;; photos
   ;; root
   ;; poll
   ))


(defmethod print-object ((obj post) stream)
  (print-unreadable-object (obj stream :type t)
    ;; fragile ?
    (format stream "id ~a '~a'" (post-id obj) (str:substring 0 15 (title obj)))))

(defun get-stream ()
  (let ((content
         (decode-json-from-string
          (dex:get (str:concat *pod* "/stream.json")
                   :cookie-jar *session-cookie*
                   :verbose t))))
    (setf *json-stream*
          (loop for it in content
             :collect
               (make-instance 'post
                              :id (assoc-value :id it)
                              :guid (assoc-value :guid it)
                              :created-at (assoc-value :created-at it)
                              :title (assoc-value :title it)
                              :text (assoc-value :text it))))))

(defun show-stream (&key (length 10))
  (mapcar (lambda (post)
            (format t "~a~%" (text post))
            (format t "~&----------~%"))
          (str:substring 0 length *json-stream*)))

(defun stream-titles (&key (length 10))
  (mapcar (lambda (post)
            (format t "~a~&" (title post)))
          (str:substring 0 length *json-stream*)))

(defgeneric text (obj)
  (:documentation "Get the text of this post."))

(defmethod text ((post t))
  (assoc-value :text post))

(defmethod text ((post post))
  (slot-value post 'text))

(defmethod (setf text) (val (post post))
  (setf (slot-value post 'text) val))
