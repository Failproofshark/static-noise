(in-package :cl-user)

(defpackage :static-noise
  (:use :cl
        :cl-ppcre
        :cl-who
        :djula
        :cl-fad
        :cl-markdown
        :hunchentoot
        :alexandria
        :local-time)
  (:shadowing-import-from :djula :url-encode)
  (:shadowing-import-from :cl-fad :copy-file :copy-stream))
(in-package :static-noise)

(defvar *blog-directory* 'nil)
(defvar *article-template* 'nil)
(defvar *archive-template* 'nil)
(defvar *page-template* 'nil)
(defvar *rss-feed-template* 'nil)
(defvar *blog-title* 'nil)
(defvar *blog-description* 'nil)
(defvar *blog-url* 'nil)
(defvar *content-cache* 'nil)
(defvar *page-cache* 'nil)

(defvar *server* (make-instance 'acceptor :port 8080))

(defun copy-directory (from-directory to-directory)
  (ensure-directories-exist to-directory)
  (walk-directory from-directory
                  #'(lambda (original-file)
                      (let* ((sub-directory-split (split (namestring from-directory) (namestring original-file)))
                             (partial-path (if (> (- (length sub-directory-split) 1) 0)
                                               (nth (- (length sub-directory-split) 1) sub-directory-split)
                                               'nil)))
                        (when partial-path
                            (if (and (directory-pathname-p partial-path)
                                     (not (string= partial-path "/")))
                                (ensure-directories-exist (merge-pathnames-as-directory to-directory partial-path))
                                (copy-file original-file (merge-pathnames-as-file to-directory partial-path) :overwrite t)))))
                  :directories :breadth-first))

(defun set-blog-paths (blog-directory)
  (setf *blog-directory* blog-directory)
  (add-template-directory (merge-pathnames-as-directory blog-directory "templates/"))
  (setf (acceptor-document-root *server*) (merge-pathnames-as-directory blog-directory "rendered/")))

;;for configuration purposes
(defun :blog-configuration (&key article archive page rss-template title description url)
  (when (and article archive page rss-template title description url)
    (progn
      (setf *article-template* article)
      (setf *archive-template* archive)
      (setf *page-template* page)
      (setf *blog-title* title)
      (setf *blog-description* description)
      (setf *blog-url* url)
      (setf *rss-feed-template* rss-template))))

(defun open-blog (blog-directory)
  (if (file-exists-p (merge-pathnames-as-file blog-directory "config.lisp"))
      (let ((article-cache-file-path (merge-pathnames-as-file blog-directory "article-cache"))
            (page-cache-file-path (merge-pathnames-as-file blog-directory "page-cache")))
        (set-blog-paths blog-directory)
        (load (merge-pathnames-as-file blog-directory "config.lisp"))
        (when (file-exists-p article-cache-file-path)
          (with-open-file (article-cache-file article-cache-file-path)
            (setf *content-cache* (read article-cache-file))))
        (when (file-exists-p page-cache-file-path)
          (with-open-file (page-cache-file page-cache-file-path)
            (setf *page-cache* (read page-cache-file)))))
      (format t "No configuration file found")))

(defun create-blog (blog-directory-root blog-title blog-description blog-url)
  (ensure-directories-exist blog-directory-root)
  (ensure-directories-exist (merge-pathnames-as-directory blog-directory-root "templates/"))
  (ensure-directories-exist (merge-pathnames-as-directory blog-directory-root "content/"))
  (ensure-directories-exist (merge-pathnames-as-directory blog-directory-root "static/"))
  (ensure-directories-exist (merge-pathnames-as-directory blog-directory-root "pages/"))
  (with-open-file (new-config-file (merge-pathnames-as-file blog-directory-root "config.lisp")
                                   :direction :output
                                   :if-exists 'nil
                                   :if-does-not-exist :create)
    (print `(:blog-configuration :title ,blog-title
                                 :description ,blog-description
                                 :url ,blog-url
                                 :article "article.html"
                                 :archive "archive.html"
                                 :page "page.html"
                                 :rss-template "feed-template.xml")
           new-config-file))
  (open-blog blog-directory-root))

(defun start-dev-server ()
  (start *server*))

(defun stop-dev-server ()
  (stop *server*))

;; This extension forces cl-markdown to ignore our metadata
(defsimple-extension metadata
  "")

;; split-date-components timestamp => list
;; Returns a list of date components from a given timestamp day month year
(defun split-date-components (timestamp)
  (list :day (timestamp-day timestamp)
        :month (timestamp-month timestamp)
        :year (timestamp-year timestamp)))

;; render-page path-specifier path-specifier stream &optional extra-environment-variables => string
;; Given a path specifier to the template and related markdown content
;; produce a plist with the metacontent and the rendered page output as a plain string
(defun render-page (template-file-name stream &key content-path extra-environment-variables)
  (let ((input-content-string (make-string-output-stream)))
    (if content-path
        (markdown content-path :stream input-content-string))
    (apply #'render-template* 
           (append (list (compile-template* template-file-name)
                         stream)
                   (if content-path
                       `(:content ,(get-output-stream-string input-content-string)))
                   extra-environment-variables))))

(defun create-article-listing (blog-directory)
  "Creates an array of plists containing a list of articles, their path name and their associated meta data sorted by the date they were created."
  (flet ((split-date (date-string) (split "-" (regex-replace-all "\\s" date-string ""))))
    (let ((temp-list (sort (map 'list
                                (lambda (content-file-path)
                                  (with-open-file (content-file content-file-path)
                                    (let ((extracted-metadata (car (multiple-value-list (scan-to-strings "\\(.*\\)" (read-line content-file))))))
                                      (if extracted-metadata
                                          (let* ((metadata-object (read (make-string-input-stream extracted-metadata)))
                                                 (article-date (split-date (getf metadata-object :date-created))))
                                            ;;Two extra "attributes" we wish to add to the existing metadata we parsed
                                            (setf (getf metadata-object :file-path) content-file-path)
                                            (setf (getf metadata-object :date-created) (encode-timestamp 0 
                                                                                                         0 
                                                                                                         0 

                                                                                                         0 
                                                                                                         (parse-integer (second article-date)) 
                                                                                                         (parse-integer (first article-date)) 
                                                                                                         (parse-integer (third article-date))))
                                            metadata-object)
                                          'nil))))
                                (list-directory (merge-pathnames-as-directory blog-directory #p"content/")))
                           (lambda (article-metadata-one article-metadata-two)
                             (timestamp>= (getf article-metadata-one :date-created) (getf article-metadata-two :date-created))))))
      (make-array (list (length temp-list)) :initial-contents temp-list))))

(defun create-cache (listing)
  "Creates a plist with the structure (:filename (:content \"some_content\" :last-modified 123456)). This is usually run once with a newly created blog as it is called during the normal rendering process. The only other times it runs is either manually by the user or if the cache was somehow previously deleted"
  (loop for item across listing
     append (let ((file-path (getf item :file-path)))
              `(,(make-keyword (file-namestring file-path))
                 (:content
                  ,(cadr (multiple-value-list (markdown file-path :stream nil)))
                  :last-modified ,(file-write-date file-path))))))

(defun create-slug (metadata)
  "Creates a slug useful for creating page links and file names"
  (regex-replace-all "\\W"
                     (regex-replace-all "\\s"
                                        (car (multiple-value-list (regex-replace-all
                                                                   "\\s+"
                                                                   (string-downcase (getf metadata :title))
                                                                   " ")))
                                        "_")
                     ""))

(defun create-article-link (slug &optional blog-url)
  "Creates a link to a specific article"
  (concatenate 'string
               (when blog-url
                 blog-url)
               "/articles/"
               slug
               ".html"))

(defun render-articles (article-listing article-template blog-directory)
  "Returns the array of articles listed so methods could be chained. The point of interest is it's side effect which fills the rendered/articles directory within the blog directory with the rendered articles (and creates the sub directories if they do not already exist"
  (let ((rendered-article-path (merge-pathnames-as-directory blog-directory "rendered/articles/")))
    (ensure-directories-exist rendered-article-path)
    ;; We traverse by index rather than as across because we want access to articles before and after the
    ;; one we are currently looking at
    (loop for i from 0 upto (- (length article-listing) 1) do
         (let* ((article (aref article-listing i))
                (current-slug (create-slug article)))
           (with-open-file (outfile (merge-pathnames-as-file  rendered-article-path (concatenate 'string current-slug ".html"))
                                    :direction :output 
                                    :if-exists :rename-and-delete
                                    :if-does-not-exist :create)
             (let ((next-article-info
                    (if (> i 0)
                        (let* ((next-entry (aref article-listing (- i 1)))
                               (next-slug (create-slug next-entry)))
                          (list :next-entry-title (getf next-entry :title)
                                :next-entry (create-article-link next-slug)))))
                   (previous-article-info
                    (if (< (+ i 1) (length article-listing))
                        (let* ((previous-entry (aref article-listing (+ i 1)))
                              (previous-slug (create-slug previous-entry)))
                          (list :previous-entry-title (getf previous-entry :title)
                                :previous-entry (create-article-link previous-slug)))))
                   (date-created (split-date-components (getf article :date-created))))
               (apply #'render-page (append (list article-template
                                                  outfile)
                                            `(:content-path ,(getf article :file-path))
                                            `(:extra-environment-variables ,(append previous-article-info
                                                                                    next-article-info
                                                                                    `(:date-created ,date-created)
                                                                                    `(:article-title ,(getf article :title))))))))))))

(defun create-archive-metadata (article-listing)
  "Extracts data relavant for an archive page from a more complete metadata set"
  (loop for article across article-listing collect
       (let* ((current-slug (create-slug article))
              (article-title (getf article :title))
              (creation-date-components (split-date-components (getf article :date-created))))
         (append (list :title article-title
                       :page-link (create-article-link current-slug)
                       :date creation-date-components)))))

;; TODO Create different strategies for listings (separate by year and month)?
(defun render-simple-archive (article-listing archive-template blog-directory)
  (with-open-file (outfile (merge-pathnames-as-file blog-directory "rendered/archive.html")
                           :direction :output 
                           :if-exists :rename-and-delete
                           :if-does-not-exist :create)
    (render-page archive-template
                 outfile
                 :extra-environment-variables `(:articles ,(create-archive-metadata article-listing)))))

(defun create-page-listing (blog-directory)
  (map 'list
       #'(lambda (page-file-path)
           (with-open-file (page-file page-file-path)
             (let ((extracted-metadata (car (multiple-value-list (scan-to-strings "\\(.*\\)" (read-line page-file))))))
               (if extracted-metadata
                   (let* ((metadata-object (read (make-string-input-stream extracted-metadata))))
                     (setf (getf metadata-object :file-path) page-file-path)
                     metadata-object)
                   'nil))))
       (list-directory (merge-pathnames-as-directory blog-directory #p"pages/"))))

(defun render-rss-feed (blog-directory rss-template blog-title blog-url blog-description article-listing)
  (flet ((format-to-rfc-822 (article-date)
           (let ((day-enum #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
                 (month-enum #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
             (format 'nil
                     "~a, ~a ~a ~a 00:00:00 GMT"
                     (aref day-enum (timestamp-day-of-week article-date))
                     (timestamp-day article-date)
                     (aref month-enum (- (timestamp-month article-date) 1))
                     (timestamp-year article-date)))))
      (with-open-file (rss-feed (merge-pathnames-as-file blog-directory "rendered/rss.xml")
                                :direction :output
                                :if-exists :rename-and-delete
                                :if-does-not-exist :create)
        (render-page rss-template
                     rss-feed
                     :extra-environment-variables (append `(:blog-title ,blog-title
                                                            :blog-url ,blog-url
                                                            :blog-description ,blog-description
                                                            :last-build-date ,(format-to-rfc-822 (now))
                                                            :items)
                                                          `(,(loop for article across article-listing
                                                                collect (append `(:title ,(getf article :title)
                                                                                         :description ,(cadr (multiple-value-list (markdown (getf article :file-path) :stream 'nil)))
                                                                                         :link ,(concatenate 'string
                                                                                                            *blog-url*
                                                                                                            "/articles/"
                                                                                                            (create-slug article)
                                                                                                            ".html")
                                                                                         :publish-date ,(format-to-rfc-822 (getf article :date-created)))
                                                                                (if (getf article :category)
                                                                                    `(:category ,(getf article :category))
                                                                                    nil)))))))))
         

(defun render-pages (page-listing page-template blog-directory)
  (let ((rendered-pages-path (merge-pathnames-as-directory blog-directory #p"rendered/pages/")))
    (ensure-directories-exist rendered-pages-path)
    (loop for page in page-listing do
         (let ((page-slug (create-slug page)))
           (with-open-file (outfile (merge-pathnames-as-file rendered-pages-path (concatenate 'string page-slug ".html"))
                                    :direction :output
                                    :if-exists :rename-and-delete
                                    :if-does-not-exist :create)
             (render-page page-template
                          outfile
                          :content-path (getf page :file-path)
                          :extra-environment-variables `(:page-title ,(getf page :title))))))))

(defun render-all (blog-directory article-template archive-template page-template rss-template blog-title blog-description blog-url)
  (flet ((get-first-article-path (the-articles)
           (merge-pathnames-as-file *blog-directory*
                                    "rendered/articles/"
                                    (concatenate 'string (create-slug (aref the-articles 0)) ".html"))))
         (let* ((articles (create-article-listing blog-directory))
                (pages (create-page-listing blog-directory)))
           (when (> (length articles) 0)
             (progn (render-articles articles article-template blog-directory)
                    (render-simple-archive articles archive-template blog-directory)
                    (copy-file (get-first-article-path articles) (merge-pathnames-as-file blog-directory "rendered/index.html") :overwrite t)
                    (render-rss-feed blog-directory rss-template blog-title blog-url blog-description articles)))
           (when (> (length pages) 0)
             (render-pages pages page-template blog-directory))
           (copy-directory (merge-pathnames-as-directory blog-directory "static/") (merge-pathnames-as-directory blog-directory "rendered/" "static/")))))

(defun render-blog ()
  (render-all *blog-directory*
              *article-template*
              *archive-template*
              *page-template*
              *rss-feed-template*
              *blog-title*
              *blog-description*
              *blog-url*))
