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
(defvar *article-cache* 'nil)
(defvar *page-cache* 'nil)

(defvar *server* (make-instance 'acceptor :port 8080))

(declaim (optimize (debug 3)))

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
  (enable-read-macros)
  (if (file-exists-p (merge-pathnames-as-file blog-directory "config.lisp"))
      (let ((article-cache-file-path (merge-pathnames-as-file blog-directory "article-cache.lisp"))
            (page-cache-file-path (merge-pathnames-as-file blog-directory "page-cache.lisp")))
        (set-blog-paths blog-directory)
        (load (merge-pathnames-as-file blog-directory "config.lisp"))
        (if (file-exists-p article-cache-file-path)
            (with-open-file (article-cache-file article-cache-file-path)
              (setf *article-cache* (read article-cache-file)))
            nil)
        (if (file-exists-p page-cache-file-path)
          (with-open-file (page-cache-file page-cache-file-path)
            (setf *page-cache* (read page-cache-file)))
          nil)
        (format t "Blog has been opened"))
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

(defun split-date-components (timestamp)
  "Returns a list of date components from a given timestamp day month year"
  (list :day (timestamp-day timestamp)
        :month (timestamp-month timestamp)
        :year (timestamp-year timestamp)))

(defun render-page (template-file-name stream &key content content-path extra-environment-variables)
  "Given the template to render with a stream to write to, the actual content (content key argument) OR a path specifier (content-path key argument) render the page to HTML. Extra environment variables for the templates use may be made available by sending a list through the extra-environment-variables key argument"
  (let ((input-content-string (make-string-output-stream)))
    (if content-path
        (markdown content-path :stream input-content-string))
    (apply #'render-template* 
           (append (list (compile-template* template-file-name)
                         stream)
                   `(:content ,(if content-path
                                   (get-output-stream-string input-content-string)
                                   content))
                   extra-environment-variables))))

(defun create-content-listing (the-cache content-directory &optional extra-metadata-function)
  (labels ((render-if-new (metadata)
             (let* ((file-path (getf metadata :file-path))
                    (recorded-modified-date (getf metadata :last-modified))
                    (last-write-date (file-write-date file-path))
                    (old-content (getf metadata :content)))
               (or (and recorded-modified-date
                        (<= recorded-modified-date last-write-date)
                        old-content)
                   (multiple-value-bind (doc rendered-content) (markdown file-path :stream 'nil) (declare (ignore doc))
                                        rendered-content))))
           (populate-metadata (metadata)
             (with-open-file (page-file (getf metadata :file-path))
               (let ((extracted-metadata (car (multiple-value-list (scan-to-strings "\\(.*\\)" (read-line page-file))))))
                 (if extracted-metadata
                     (let* ((metadata-object (read (make-string-input-stream extracted-metadata))))
                       (setf (getf metadata-object :file-path) (getf metadata :file-path))
                       (setf (getf metadata-object :last-modified) (file-write-date (getf metadata :file-path)))
                       (setf (getf metadata-object :content) (render-if-new metadata-object))
                       (when extra-metadata-function
                         (apply extra-metadata-function `(,metadata)))
                       metadata-object)
                     'nil)))))
    (let* ((cache-invalid nil)
           (new-entries (map 'list
                             (lambda (metadata)
                               (let ((newly-created-metadata (populate-metadata metadata)))
                                 (setf (getf newly-created-metadata :mark-for-deletion) nil)
                                 newly-created-metadata))
                             (set-difference (map 'list
                                                  (lambda (content-file-path)
                                                    `(:file-path ,content-file-path))
                                                  (list-directory content-directory))
                                             the-cache
                                             :key (lambda (metadata)
                                                    (namestring (getf metadata :file-path)))
                                             :test #'string=)))
           (updated-cache (remove-if (lambda (metadata)
                                       (getf metadata :mark-for-deletion))
                                     (map 'list
                                          (lambda (metadata)
                                            (let ((new-metadata (copy-list metadata)))
                                              (if (probe-file (getf new-metadata :file-path))
                                                  (if (> (file-write-date (getf new-metadata :file-path)) (getf new-metadata :last-modified))
                                                      (progn
                                                        (setf cache-invalid t)
                                                        (populate-metadata new-metadata))
                                                      new-metadata)
                                                  (progn
                                                    (setf (getf new-metadata :mark-for-deletion) t)
                                                    (setf cache-invalid t)
                                                    new-metadata))))
                                          the-cache))))
      (values (concatenate 'list
                           new-entries
                           updated-cache)
              (or (> (length new-entries) 0) cache-invalid)))))

(defun create-article-listing (blog-directory article-cache)
  (sort (create-content-listing article-cache
                                (merge-pathnames-as-directory blog-directory #p"content/")
                                (lambda (metadata)
                                  (let ((article-date (split "-" (regex-replace-all "\\s" (getf metadata :date_created) ""))))
                                    (setf (getf metadata :date-created) (encode-timestamp 0 
                                                                                          0 
                                                                                          0 
                                                                                          0 
                                                                                          (parse-integer (second article-date)) 
                                                                                          (parse-integer (first article-date)) 
                                                                                          (parse-integer (third article-date))))
                                    metadata)))
        (lambda (entry-1 entry-2)
          (timestamp>= (getf entry-1 :date-created) (getf entry-2 :date-created)))))

(defun write-articles-cache (blog-directory article-listing should-write-cache)
  (when (or (not (file-exists-p (merge-pathnames-as-file blog-directory "article-cache.lisp")))
            should-write-cache)
    (with-open-file (new-cache-file (merge-pathnames-as-file blog-directory "article-cache.lisp")
                                      :direction :output
                                      :if-exists :rename-and-delete
                                      :if-does-not-exist :create)
      (print article-listing new-cache-file))
    (setf *article-cache* article-listing)))

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
  (let* ((rendered-article-path (merge-pathnames-as-directory blog-directory "rendered/articles/"))
         (next-article nil))
    (progn
      (ensure-directories-exist rendered-article-path)
      (loop for current-article in article-listing collect
           (let ((current-slug (create-slug current-article))
                 (previous-article (cadr article-listing)))
             (with-open-file (outfile (merge-pathnames-as-file rendered-article-path (concatenate 'string current-slug ".html"))
                                      :direction :output 
                                      :if-exists :rename-and-delete
                                      :if-does-not-exist :create)
               (let ((next-article-info
                      (if next-article
                          (list :next-entry-title (getf next-article :title)
                                :next-entry (create-article-link (create-slug next-article)))))
                     (previous-article-info
                      (if previous-article
                          (list :previous-entry-title (getf previous-article :title)
                                :previous-entry (create-article-link (create-slug previous-article)))))
                     (date-created (split-date-components (getf current-article :date-created))))
                 (apply #'render-page (append `(,article-template
                                                ,outfile
                                                :content ,(getf current-article :content))
                                              `(:extra-environment-variables ,(append previous-article-info
                                                                                      next-article-info
                                                                                      `(:date-created ,date-created)
                                                                                      `(:article-title ,(getf current-article :title))))))
                 (setf next-article current-article)
                 (setf (getf current-article :last-modified)
                       (file-write-date (getf current-article :file-path)))))
             current-article)))))

(defun create-archive-metadata (article-listing)
  "Extracts data relavant for an archive page from a more complete metadata set"
  (loop for article in article-listing collect
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

(defun create-page-listing (blog-directory page-cache)
  (create-content-listing page-cache (merge-pathnames-as-directory blog-directory #p"pages/")))

(defun write-page-cache (blog-directory page-listing should-write-cache)
  (when (or (not (file-exists-p (merge-pathnames-as-file blog-directory "page-cache.lisp")))
            should-write-cache)
    (with-open-file (new-cache-file (merge-pathnames-as-file blog-directory "page-cache.lisp")
                                      :direction :output
                                      :if-exists :rename-and-delete
                                      :if-does-not-exist :create)
      (print page-listing new-cache-file))
    (setf *page-cache* page-listing)))

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
                                                          `(,(loop for article in article-listing
                                                                collect (append `(:title ,(getf article :title)
                                                                                         :description ,(cadr (multiple-value-list (markdown (getf article :file-path) :stream 'nil)))
                                                                                         :link ,(create-article-link (create-slug article) blog-url)
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
                          :content (getf page :content)
                          :extra-environment-variables `(:page-title ,(getf page :title))))))))

(defun render-all (blog-directory article-template article-cache archive-template page-template page-cache rss-template blog-title blog-description blog-url)
  (flet ((get-first-article-path (the-articles)
           (merge-pathnames-as-file blog-directory
                                    "rendered/articles/"
                                    (concatenate 'string (create-slug (first the-articles)) ".html")))
         (create-listing-and-write-cache (listing-creation-function cache-writing-function current-cache)
           (multiple-value-bind (listing is-invalid) (apply listing-creation-function `(,blog-directory ,current-cache))
             (apply cache-writing-function `(,blog-directory ,listing ,is-invalid))
             listing)))
    (let* ((articles (create-listing-and-write-cache #'create-article-listing
                                                     #'write-articles-cache
                                                     article-cache))
           (pages (create-listing-and-write-cache #'create-page-listing
                                                  #'write-page-cache
                                                  page-cache)))
      (when (> (length articles) 0)
        (render-articles articles
                         article-template
                         blog-directory)
        (render-simple-archive articles
                               archive-template
                               blog-directory)
        (copy-file (get-first-article-path articles) (merge-pathnames-as-file blog-directory "rendered/index.html") :overwrite t)
        (render-rss-feed blog-directory rss-template
                         blog-title
                         blog-url
                         blog-description
                         articles))
      (when (> (length pages) 0)
        (render-pages pages
                      page-template
                      blog-directory))
      (copy-directory (merge-pathnames-as-directory blog-directory "static/") (merge-pathnames-as-directory blog-directory "rendered/" "static/")))))

(defun render-blog ()
  (render-all *blog-directory*
              *article-template*
              *article-cache*
              *archive-template*
              *page-template*
              *page-cache*
              *rss-feed-template*
              *blog-title*
              *blog-description*
              *blog-url*))
