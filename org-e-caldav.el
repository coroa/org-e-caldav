;;; org-e-caldav.el --- Sync org files with external calendar through CalDAV

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Jonas Hörsch <coroa@online.de>
;; Keywords: calendar, caldav
;;
;; This file is not part of GNU Emacs.
;;
;; org-e-caldav.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; org-e-caldav.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Assumptions
;; 
;; - The UID and the resource name are the same
;;
;; - There is only one event per resource
;; 
;; This package depends on the url-dav package, which unfortunately is
;; broken in Emacs proper. Get a fixed one from
;;   https://github.com/dengste/org-caldav
;; and load it before using org-e-caldav.
;;
;; In a nutshell:
;;
;; - Create a new calendar; the name does not matter. Again, do *not*
;;   use your precious main calendar.
;;
;; - Set `org-e-caldav-url' to the calendar address of your CalDAV
;;   server, it is usually made up of a base url + a calendar id
;;    * Owncloud: https://OWNCLOUD-SERVER-URL/remote.php/caldav/calendars/USERID/CALENDAR/
;;    * Google: https://www.google.com/calendar/dav/ID@group.calendar.google.com/events/
;;      where the ID is shown in the 'calendar settings' on Google.
;;
;; - Set `org-e-caldav-files' to the list of org files you would like to
;;   sync. The inbox file is added automatically.
;;
;; - Set `org-e-caldav-inbox' to an org filename where new entries from
;;   the calendar should be stored.
;;
;; Call `org-e-caldav-sync' to start the sync. The URL package will ask
;; you for username/password for accessing the calendar.


;;; Code:

(require 'url-dav)
(require 'icalendar)
(require 'org-element)
(require 'org-e-icalendar)

(defvar org-e-caldav-url (concat "https://www.google.com/calendar/dav/"
                                 "ID@group.calendar.google.com"
                                 "/events/")
  "Calendar URL for CalDAV access. The following '/' is mandatory.")

(defvar org-e-caldav-files '("~/org/appointments.org")
  "List of files which should end up in calendar.
`org-e-caldav-inbox' is added automatically.")

(defvar org-e-caldav-inbox "~/org/from-calendar.org"
  "Filename for putting new entries obtained from calendar.")

(defvar org-e-caldav-debug t)
(defvar org-e-caldav-debug-buffer "*org-e-caldav-debug*")

;; Variables users usually should not have to touch

(defvar org-e-caldav-uid-property "CALDAVUID"
  "Property name in which to save the associated caldav uid.")

(defvar org-e-caldav-conflicts-buffer "*Org-e-caldav conflicts*"
  "Name of the conflict buffer")

;; Internal state variables

;;; The following state definitions and functions come literally from
;;; os.el, written by Aurélien Aptel

(defvar org-e-caldav-state-file (concat user-emacs-directory "org-e-caldav-state")
  "Path to org-e-caldav state file.")

(defvar org-e-caldav-state-alist nil)

(defun org-e-caldav-set-state (file eventlist &optional uidsreset)
  "Update FILE to EVENTLIST in `org-e-caldav-state-alist'. Events
with uids in UIDSRESET will be reset to the previous saved
state. Further is the :headline element removed from all events
and the :parent property of the timestamp element."
  (when uidsreset
    (let ((state (org-e-caldav-get-state file)))
      (mapc (lambda (uid)
              (let ((lpair (assoc uid (plist-get eventlist :events)))
                    (spair (assoc uid (plist-get state :events))))
                (if spair
                    (setcdr lpair (cdr spair))
                  (plist-put eventlist :events
                             (delq lpair (plist-get eventlist :events))))))
            uidsreset)))
  
  (loop for pair in (plist-get eventlist :events)
        as (uid . event) = pair
        do
        (org-element-put-property (plist-get event :timestamp)
                                  :parent nil)
        (plist-put event :headline nil)
        unless uid
        do (setcar pair (plist-get event :uid)))

  (setq eventlist
        (plist-put eventlist :date-state (current-time)))

  (let ((cell (assoc file org-e-caldav-state-alist)))
    (if cell
        (setcdr cell eventlist)
      (push (cons file eventlist) org-e-caldav-state-alist))))

(defun org-e-caldav-get-state (file)
  "Return the EVENTLIST for FILE in state or nil."
    (cdr (assoc file org-e-caldav-state-alist)))

(defun org-e-caldav-write-state ()
  "Write org-e-caldav state to `org-e-caldav-state-file'."
  (with-temp-file org-e-caldav-state-file
    (prin1 `(setq org-e-caldav-state-alist ',org-e-caldav-state-alist)
           (current-buffer))))

(defun org-e-caldav-load-state ()
  "Load org-e-caldav state from `org-e-caldav-state-file'."
  (load org-e-caldav-state-file 'noerror nil))

;;; A few functions from org-caldav.el by David Engster

(defun org-e-caldav-check-connection ()
  "Check connection by doing a PROPFIND on CalDAV URL."
  (org-e-caldav-debug-print (format "Check connection - doing a PROPFIND on %s."
				  org-e-caldav-url))
  (let ((output (url-dav-request org-e-caldav-url "PROPFIND" nil nil 1)))
  (unless (eq (plist-get (cdar output) 'DAV:status) 200)
    (org-e-caldav-debug-print "Got error status from PROPFIND: " output)
    (error "Could not query CalDAV URL %s." org-e-caldav-url)))
  t)

(defun org-e-caldav-debug-print (&rest objects)
  "Print OBJECTS into debug buffer if `org-e-caldav-debug' is non-nil."
  (when org-e-caldav-debug
    (with-current-buffer (get-buffer-create org-e-caldav-debug-buffer)
      (dolist (cur objects)
	(if (stringp cur)
	    (insert cur)
	  (prin1 cur (current-buffer)))
	(insert "\n")))))

;;; The following is taken to large parts from icalendar.el, written
;;; by Ulf Jasper.

(defun org-e-caldav-ical-to-event (uid)
  "Convert icalendar event in current buffer.
Returns a list '(start-d start-t end-d end-t summary description)'
which can be fed into `org-e-caldav-insert-org-entry'."
  (decode-coding-region (point-min) (point-max) 'utf-8-auto)
  (goto-char (point-min))
  (let* ((calendar-date-style 'european)
	 (ical-list (icalendar--read-element nil nil))
	 (e (car (icalendar--all-events ical-list)))
	 (zone-map (icalendar--convert-all-timezones ical-list))
	 (dtstart (icalendar--get-event-property e 'DTSTART))
	 (dtstart-zone (icalendar--find-time-zone
			(icalendar--get-event-property-attributes
			 e 'DTSTART)
			zone-map))
	 (dtstart-dec (icalendar--decode-isodatetime dtstart nil
						     dtstart-zone))
	 (dtend (icalendar--get-event-property e 'DTEND))
	 (dtend-zone (icalendar--find-time-zone
		      (icalendar--get-event-property-attributes
		       e 'DTEND)
		      zone-map))
	 (dtend-dec (icalendar--decode-isodatetime dtend
						   nil dtend-zone))
	 (dtend-1-dec (icalendar--decode-isodatetime dtend -1
						     dtend-zone))
	 (summary (icalendar--convert-string-for-import
		   (or (icalendar--get-event-property e 'SUMMARY)
		       "No Title")))
	 (description
          (replace-regexp-in-string
           "\n " "\n" (icalendar--convert-string-for-import
                       (or (icalendar--get-event-property e 'DESCRIPTION)
                           "")) t t))
	 (rrule (icalendar--get-event-property e 'RRULE))
	 (rdate (icalendar--get-event-property e 'RDATE))
	 (duration (icalendar--get-event-property e 'DURATION))
         start-t end-t range repeater timestamp)

    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
			  dtstart-dec
			  (icalendar--decode-isoduration duration)))
	    (dtend-1-dec-d (icalendar--add-decoded-times
			    dtstart-dec
			    (icalendar--decode-isoduration duration
							   t))))
	(if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
	    (message "Inconsistent endtime and duration for %s"
		     summary))
	(setq dtend-dec dtend-dec-d)
	(setq dtend-1-dec dtend-1-dec-d)))
    
    (setq start-t
          (unless (and dtstart
                       (string=
                        (cadr (icalendar--get-event-property-attributes
                               e 'DTSTART))
                        "DATE"))
            `(:hour-start ,(nth 2 dtstart-dec)
                          :minute-start ,(nth 1 dtstart-dec)))
          end-t
          (if (and dtend
                   (string=
                    (cadr (icalendar--get-event-property-attributes
                           e 'DTEND))
                    "DATE"))
              (when start-t
                `(:hour-end ,(plist-get start-t :hour-start)
                            :minute-end ,(plist-get start-t :minute-start)))
            `(:hour-end ,(nth 2 dtend-dec)
                        :minute-end ,(nth 1 dtend-dec)))
          dtend-dec (if end-t dtend-dec dtend-1-dec)
          range (not (equal dtstart-dec dtend-dec))
          repeater
          (when rrule
            (let* ((rrule-props (icalendar--split-value rrule))
                   (frequency   (cadr (assoc 'FREQ rrule-props)))
                   (interval    (read (or (cadr (assoc 'INTERVAL rrule-props)) "1"))))
              `(:repeater-type
                cumulate
                :repeater-unit
                ,(cdr (assoc-string frequency
                                    '(("HOURLY" . hour) ("DAILY" . day) ("WEEKLY" . week)
                                      ("MONTHLY" . month) ("YEARLY" . year))))
                :repeater-value
                ,interval)))
          timestamp
          `(timestamp
            (:type ,(if range 'active-range 'active)
                   :year-start ,(nth 5 dtstart-dec)
                   :month-start ,(nth 4 dtstart-dec)
                   :day-start ,(nth 3 dtstart-dec)
                   ,@start-t
                   :year-end ,(nth 5 dtend-dec)
                   :month-end ,(nth 4 dtend-dec)
                   :day-end ,(nth 3 dtend-dec)
                   ,@end-t
                   ,@repeater)))

    `(:uid ,uid
           :timestamp ,timestamp
           :summary ,summary
           :description ,description)))

(defun org-e-caldav-event-to-ical (event)
  "Generate and return the ical equivalent to the event structure
event."
  (let ((description (replace-regexp-in-string
                      "\n" "\\n" (plist-get event :description) t t)))

    (concat "BEGIN:VCALENDAR\n"
            (org-e-icalendar--vevent (plist-get event :headline)
                                     (plist-get event :timestamp)
                                     (plist-get event :uid)
                                     (plist-get event :summary)
                                     nil
                                     description
                                     nil)
            "END:VCALENDAR\n")))

(defun org-e-caldav-fetch-event (uid state)
  (let* ((last-update (plist-get state :date-state))
         (old-pair (assoc uid (plist-get state :events)))
         (url-request-extra-headers
          (when (and last-update old-pair)
            `(("If-Modified-Since" . ,(url-get-normalized-date last-update))))))
    
    (with-current-buffer (flet ((url-cache-extract (x) nil))
                           (url-retrieve-synchronously
                            (concat org-e-caldav-url (concat uid ".ics"))))
      (case url-http-response-status
        (304 old-pair)
        (200 (delete-region
              (point-min)
              (progn (goto-char url-http-end-of-headers)
                     (search-forward "BEGIN:VCALENDAR")
                     (match-beginning 0)))
             (cons uid (org-e-caldav-ical-to-event uid)))
        (404 (cons uid nil))
        (t   (error "Couldn't retrieve event"))))))

(defun org-e-caldav-fetch-eventlist ()
  "Fetch list of events from remote calendar."
  (mapcar 'file-name-sans-extension
          (url-dav-directory-files org-e-caldav-url nil "\\.ics$")))

(defun org-e-caldav-find-property-drawer (elem)
  "Return the first property drawer directly under headline."
  (if (eq (org-element-type elem) 'property-drawer)
      elem
    (org-element-map (cons 'org-data (cdr elem))
                     'property-drawer
                     'identity
                     nil
                     t
                     (org-element-type elem))))

(defun org-e-caldav-property-drawer-to-alist (drawer)
  "Find a property drawer and return its properties as an alist."
  (org-element-map (org-e-caldav-find-property-drawer drawer)
                   'node-property
                   (lambda (x) (cons (org-element-property :key x)
                                (org-element-property :value x)))))

(defun org-e-caldav-alist-to-property-drawer (alist &optional elem)
  "Return the property drawer corresponding to an alist of key
value pairs. If an org-element-node is passed in as drawer,
update the first found property drawer underneath."
  
  (if (null elem)
      `(property-drawer nil
                        ,@(mapcar
                          (lambda (x) `(node-property (:key ,(car x) :value ,(cdr x))))
                          alist))
    (let* ((drawer (org-e-caldav-find-property-drawer elem))
           (added (make-hash-table)))
      (apply 'org-element-set-contents drawer
             (loop for elem in (cddr drawer)
                   as x = (assoc (org-element-property :key elem) alist)
                   if (null x)
                   collect elem
                   else if (cdr x) do
                   (org-element-put-property elem :value (cdr x))
                   (puthash (car x) t added)
                   and collect elem))

      (apply 'org-element-adopt-elements drawer
             (loop for (key . val) in alist
                   unless (or (null val) (gethash key added))
                   collect `(node-property (:key ,key :value ,val)))))))

(defun org-e-caldav-event-to-headline (event &optional headline level)
  "Create a new org-element headline or update an existing
one. Returns the innermost changed org-element structure."
  (let ((drawer-alist `((,org-e-caldav-uid-property . ,(plist-get event :uid))
                        ("sync" . ,(plist-get event :sync))))
        (summary (plist-get event :summary))
        (description (plist-get event :description))
        (timestamp (plist-get event :timestamp)))

    (if (null headline)
        (if (numberp level)
            `(headline (:title ,summary :level ,level)
                       (section nil                          
                                ,(org-e-caldav-alist-to-property-drawer drawer-alist)
                                ,timestamp
                                ,description))
          (error "Need a headline level to create a new headline."))
      
      (let* ((section (org-element-map (cons 'org-data (cdr headline))
                                       'section 'identity nil t 'headline))
             basket (basket-add (lambda (x) (push x basket)))
             (old-description (org-e-caldav-extract-description-from-section section basket-add))
             (old-drawer (assoc 'property-drawer basket))
             (new-drawer (org-e-caldav-alist-to-property-drawer drawer-alist old-drawer))
             (old-timestamp (assoc 'timestamp basket))
             (new-contents (if (equal description old-description)
                               (org-element-contents section)
                             (nconc (nreverse basket) (list description)))))

        (unless (equal timestamp old-timestamp)
          (org-element-put-property timestamp :parent (org-element-property :parent old-timestamp))
          (setcdr old-timestamp (cdr timestamp)))

        (unless (org-element-property :parent new-drawer)
          (push (org-element-put-property new-drawer :parent section) new-contents))

        (apply 'org-element-set-contents section new-contents)
        
        (if (not (equal summary (org-e-caldav-strip-text-properties
                                 (car (org-element-property :title headline)))))
            (org-element-put-property headline :title summary)
          section)))))

(defvar org-e-caldav-normalize-description-restriction
  '((section paragraph plain-list)
    (paragraph)
    (plain-list item)
    (item paragraph plain-list)))

(defun org-e-caldav-normalize-description (elem &optional basket-add)
  "Constructs a minimal representation of the same object using
  org-e-caldav-restriction and caches it in the org-e-caldav-
  normalized-element-cache hash."
  (let* ((type (car elem))
         (properties (cadr elem))
         (contents (cddr elem))
         (restriction (assoc type org-e-caldav-normalize-description-restriction)))
    (cons type
          (cons
           properties
           (loop for c in contents
                 if (stringp c)
                 collect (org-e-caldav-strip-text-properties c)
                 else if (memq (car c) (cdr restriction))
                 collect (org-e-caldav-normalize-description c basket-add)
                 else if basket-add
                 do (funcall basket-add c))))))

(defun org-e-caldav-extract-description-from-section (section &optional basket-add)
  "Return the interpreted first section."
  (org-element-interpret-data
   (org-e-caldav-normalize-description section basket-add)))

(defun org-e-caldav-strip-text-properties (text)
  "Remove the text properties connected to a string object."
  (let ((ret (copy-sequence text)))
    (set-text-properties 0 (length ret) nil ret)
    ret))

(defun org-e-caldav-element-copy (element parent)
  "Return a deep copy of an org-element element."
  (cond
   ((stringp element)
    (let ((ret (copy-sequence element)))
      (put-text-property 0 (length ret) :parent parent ret)
      ret))
   ((and (consp element)
         (symbolp (car element)))
    (let* ((type (car element))
           (props (copy-sequence (cadr element)))
           (ret (list type props)))
      (plist-put props :parent parent)
      (when (eq type 'headline)
        (plist-put props :title
                   (mapcar (lambda (child) (org-e-caldav-element-copy child ret))
                           (plist-get props :title))))
      (setf (cddr ret)
            (mapcar (lambda (child) (org-e-caldav-element-copy child ret))
                    (cddr element)))
      ret))
   (t
    (error (concat "copy-element shouldn't be called on sequences,"
                   "which are no org-element elements")))))

(defun org-e-caldav-headline-to-event (headline timestamp)
  "Return event structure for a headline. An additional
property :headline contains the associated org-element structure."
  (let* ((prop-alist (org-e-caldav-property-drawer-to-alist headline))
         (uid (cdr (assoc org-e-caldav-uid-property prop-alist))))
    (cons uid
          `(:uid ,uid
                 :timestamp ,timestamp
                 :summary ,(org-e-caldav-strip-text-properties
                            (car (org-element-property :title headline)))
                 :description ,(org-e-caldav-extract-description-from-section
                                (car (org-element-contents headline)))
                 :sync ,(cdr (assoc "sync" prop-alist))
                 :headline ,headline))))

(defun org-e-caldav-element-closest (elem types fun)
  "Traverse upwards through its ancestors, stop at the first
match for which FUN doesn't return nil and return that value."
  (unless (listp types) (setq types (list types)))
  (let ((parent (org-element-property :parent elem)))
    (when parent
      (or (and (memq (org-element-type parent) types)
               (funcall fun parent))
          (org-e-caldav-element-closest parent types fun)))))

(defun org-e-caldav-find-events (doc)
  "Find a list of events in the current buffer. An event is
represented by a headline containing an active timestamp."
  `(:events
    ,(mapcar (lambda (ts) (org-e-caldav-headline-to-event
                      (org-e-caldav-element-closest ts 'headline 'identity) ts))
             (org-element-map doc
                              'timestamp
                              (lambda (x)
                                (when (memq (org-element-property :type x)
                                            '(active active-range)) x))))))

(defun org-e-caldav-element-update-buffer (updates local-doc)
  "Updates the current buffer according to the list updates,
which has the form '( elem1 elem2 elem3 ... ). Nested elements
are ignored.

Assumes the :begin :end properties of the elems correspond to the
buffer state.

XXX overlays have to be treated specifically, f.ex. refer to
org-element-swap-A-B."
  (save-excursion
    ;; if the :begin or :end property for any element is missing,
    ;; replace the whole buffer
    (if (loop for elem in updates
              unless (and (numberp (org-element-property :begin elem))
                          (numberp (org-element-property :end elem)))
              return t)
        (progn
          (delete-region (point-min) (point-max))
          (insert (org-element-interpret-data local-doc)))

      ;; else it's hopefully faster to replace only the changed parts
      (let ((sorted-by-beg (sort (copy-sequence updates)
                                 (lambda (a b) (>= (org-element-property :begin a)
                                              (org-element-property :begin b)))))
            (sorted-by-end (sort updates
                                 (lambda (a b) (>= (org-element-property :end a)
                                              (org-element-property :end b))))))

        (while sorted-by-end
          (let* ((elem (car sorted-by-end))
                 (skip (1+ (position elem sorted-by-beg)))
                 (beg (org-element-property :begin elem))
                 (end (org-element-property :end elem)))
            
            (setq sorted-by-end (nthcdr skip sorted-by-end)
                  sorted-by-beg (nthcdr skip sorted-by-beg))
            
            (goto-char beg)
            (delete-region beg end)
            (insert (org-element-interpret-data elem))))))))


(defun org-e-caldav-timestamp-diff (a b)
  "Do timestamps a and b differ?"
  (loop for key in '(:type
                     :year-begin
                     :month-begin
                     :day-begin
                     :hour-begin
                     :minute-begin
                     :year-end
                     :month-end
                     :day-end
                     :minute-end
                     :repeater-unit
                     :repeater-value)
        unless (equal (org-element-property key a)
                      (org-element-property key b))
        return t))

(defun org-e-caldav-event-diff (a b)
  "Return an alist of properties that differ in A and B or nil if A = B.
The form of the alist is ((:property . (valueA valueB)...)"
  (append
   (let ((ta (plist-get a :timestamp))
         (tb (plist-get b :timestamp)))
     (when (org-e-caldav-timestamp-diff ta tb)
       `((:timestamp . (,ta ,tb)))))
   (loop for key in '(:summary :description)
         as va = (plist-get a key)
         as vb = (plist-get b key)
         unless (equal va vb)
         collect `(,key . (,va ,vb)))))

(defun org-e-caldav-eventlist-diff (a b)
  "Return a diff eventlist which turns eventlist A to B when applied."
  (let* ((a-evs (plist-get a :events))
         (b-evs (plist-get b :events))
         (uid-list
          (append
           (loop for (uid . ev) in a-evs collect uid)
           (loop for (uid . ev) in b-evs
                 if uid collect uid))))
    (delete-dups uid-list)

    (append
     (loop for uid in uid-list
           as aev = (cdr (assoc uid a-evs))
           as bev = (cdr (assoc uid b-evs))
           if (or (null aev)
                  (null bev)
                  (org-e-caldav-event-diff aev bev))
           collect (cons uid bev))
     (loop for (uid . ev) in b-evs
           if (null uid) collect (cons nil ev)))))

(defun org-e-caldav-prepare-merge (local remote luidlist-add delete-add)
  "From LOCAL diff and REMOTE diff compute the necessary local
and remote changes and sort them into a plist with the
structure (:local-updates (..)
           :remote-updates (..)
           :conflicts (..))
where the ev are normal events."
  (let ((added (make-hash-table))
        (ruidlist (org-e-caldav-fetch-eventlist))
        lups rups conflicts)

    ;; add all remote bugs
    (loop for (uid . lev) in local do
          (let* ((rpair (assoc uid remote))
                 (rev (cdr rpair)))

            (when (null uid)
              (setq uid (loop for uid = (concat (format-time-string "%Y%m%dT%H%M%SZ" nil t)
                                                (format "-%d@%s" (random 10000) system-name))
                              while (memq uid ruidlist)
                              finally return uid))
              (plist-put lev :uid uid)
              (funcall luidlist-add uid))

            ;; if there's a local event with the same uid, we have a
            ;; conflict

            ;; if the local event has a sync prop, it was merged by
            ;; the user, so we keep the local one (which might be the
            ;; remote from a previous sync)

            (cond
             ((and rpair
                   (null (plist-get lev :sync))
                   (org-e-caldav-event-diff lev rev))
              (push (cons (plist-put (copy-sequence lev) :sync 'conflict-local)
                          (plist-put (copy-sequence rev) :sync 'conflict-remote))
                    conflicts))
             ((null lev)
              (funcall delete-add uid))
             (t
              (plist-put lev :sync nil)
              (push lev rups)
              (push lev lups)))
            
            ;; mark it
            (puthash uid t added)))

    ;; add changed remote events which are the unmarked events in local
    (loop for (uid . rev) in remote do
          (when (and uid (not (gethash uid added)))
            (push (or rev `(:uid ,uid :delete t)) lups)))
    
    `(:local-updates ,lups
                     :remote-updates ,rups
                     :conflicts ,conflicts)))

(defun org-e-caldav-eventlist-dups (eventlist)
  "Return non-nil if EVENTLIST contains events with the same UID.
The value returned is a list of duplicated ids."
  (let ((hash (make-hash-table :test 'equal))
        (dups))
    (mapc (lambda (x)
            (let ((uid (car x)))
              (when uid
                (puthash uid (cons (cdr x) (gethash uid hash)) hash))))
          (plist-get eventlist :events))
    (maphash (lambda (uid events)
               (when (> (length events) 1)
                 (push (cons uid events) dups))) hash)
    dups))

(defun org-e-caldav-merge-remote (event uidsreset-add)
  "Write a local change to a remote resource."
  (let* ((uid (plist-get event :uid))
         (headline (plist-get event :headline)))

    (org-e-caldav-debug-print (format "Putting event \"%s\" UID %s.\n"
                                      (plist-get event :summary) uid))
    (unless (url-dav-save-resource
             (concat org-e-caldav-url uid ".ics")
             (encode-coding-string
              (org-e-caldav-event-to-ical event)
              'utf-8-dos) "text/calendar; charset=UTF-8")
      (funcall uidsreset-add uid)
      (warn "Couldn't upload event %s. Server problems?" uid))))

(defun org-e-caldav-merge-local (event local local-doc-updates-add)
  "Update the local-doc org-element tree to reflect the remote
changes."
  (let* ((uid (plist-get event :uid))
         (lpair (assoc uid (plist-get local :events)))
         (lev (cdr lpair))
         (headline (or (plist-get lev :headline)
                       (plist-get event :headline))))

    (funcall local-doc-updates-add
             (if (plist-get event :delete)
                 (let ((parent (org-element-property :parent headline)))
                   (org-e-caldav-debug-print (format "Deleting local headline \"%s\" with UID %s.\n"
                                                     (plist-get lev :summary) uid))    
                   (plist-put local :events (delq lpair (plist-get local :events)))
                   (when parent
                     (apply 'org-element-set-contents parent
                            (delq headline (org-element-contents parent)))
                     parent))
               (org-e-caldav-debug-print (format "Updating local headline \"%s\" with UID %s.\n"
                                                 (plist-get event :summary) uid))
               (when lpair
                 (setcdr lpair event))
               (org-e-caldav-event-to-headline event headline)))))


(defun org-e-caldav-merge-conflict (conflict uidsreset-add local-doc-updates-add)
  "For a conflict, add the two conflicting headlines next to each
other to the org-element tree."
  (let* ((lev (car conflict))
         (rev (cdr conflict))
         (lheadline (plist-get lev :headline))
         (parent (org-element-property :parent lheadline))
         (lheadlinec (memq lheadline
                           (org-element-contents parent)))
         (rheadline (org-e-caldav-element-copy lheadline parent)))

    (org-e-caldav-debug-print
     (format "Adding conflicting remote headline \"%s\" with UID %s.\n"
             (plist-get lev :summary) (plist-get lev :uid)))
    
    ;; insert rheadline in linked list after lheadline
    (setcdr lheadlinec (cons rheadline (cdr lheadlinec)))

    (org-e-caldav-event-to-headline rev rheadline)
    (funcall local-doc-updates-add
             (org-element-put-property rheadline :begin
                                       (org-element-property :end rheadline)))
    
    (funcall local-doc-updates-add
             (org-e-caldav-event-to-headline lev lheadline))

    (funcall uidsreset-add (plist-get lev :uid))))

(defun org-e-caldav-sync-file (file updated-append delete-add)
  (org-e-caldav-debug-print "Starting syncing FILE %s.\n" file)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (let* ((local-doc (org-element-parse-buffer))
             local-doc-updates
             (local-doc-updates-add (lambda (x) (push x local-doc-updates)))
             (local (org-e-caldav-find-events local-doc)))
        
        (when (org-e-caldav-eventlist-dups local)
          (error "Sync file \"%s\" contains unmerged events." file))

        ;; local          state          remote
        ;;    \          /    \          /
        ;;    parse    load   load     fetch
        ;;      \      /        \      /
        ;;     local-diff      remote-diff
        ;;             \        /
        ;;              \      /
        ;;               merge
        ;;              /     \
        ;;             /    merge-remote  --> push changes to caldav  
        ;;            |        |
        ;;      merge-local ------- - - - --> update local org-element tree
        ;;              \     /
        ;;               local
        ;;                 v
        ;;             new state

        (let* ((state (org-e-caldav-get-state file))
               (luidlist (delq nil (mapcar (lambda (x) (car x)) (plist-get local :events))))
               (luidlist-add (lambda (x) (push x luidlist)))
               uidsreset (uidsreset-add (lambda (x) (push x uidsreset)))
               (remote
                `(:events
                  ,(mapcar (lambda (x) (org-e-caldav-fetch-event x state)) luidlist)))
               (local-diff (org-e-caldav-eventlist-diff state local))
               (remote-diff (org-e-caldav-eventlist-diff state remote))
               (merge (org-e-caldav-prepare-merge local-diff remote-diff
                                                  luidlist-add delete-add)))

          (mapc  (lambda (x) (org-e-caldav-merge-remote x uidsreset-add))
                 (plist-get merge :remote-updates))
          (mapc (lambda (x) (unless (memq (plist-get x :uid) uidsreset)
                         (org-e-caldav-merge-local x local local-doc-updates-add)))
                (plist-get merge :local-updates))

          (mapc (lambda (x) (org-e-caldav-merge-conflict x uidsreset-add local-doc-updates-add))
                (plist-get merge :conflicts))

          (org-e-caldav-element-update-buffer local-doc-updates local-doc)

          (org-e-caldav-set-state file local uidsreset)
          (message "Synchronization of file \"%s\" complete." file)

          (funcall updated-append luidlist)
          (plist-get merge :conflicts))))))

(defun org-e-caldav-merge-new-local (event state local-doc-updates-add inbox level)
  "Update the local-doc org-element tree to include the new
remote event."
  (funcall local-doc-updates-add
           (org-element-adopt-elements
            inbox
            (org-e-caldav-event-to-headline event nil level)))

  (plist-put state :events
             (acons uid event (plist-get state :events))))

(defun org-e-caldav-sync-new-remotes (inbox-file filter-updated)
  (with-current-buffer (find-file-noselect inbox-file)
    (save-excursion
      (let* ((local-doc (org-element-parse-buffer))
             local-doc-updates
             (local-doc-updates-add (lambda (x) (push x local-doc-updates)))
             (state (or (org-e-caldav-get-state inbox-file)
                        '(:events nil))))

        (loop for uid in (funcall filter-updated (org-e-caldav-fetch-eventlist))
              as (ruid . ev) = (org-e-caldav-fetch-event uid state) do
              (org-e-caldav-merge-new-local ev state local-doc-updates-add local-doc 1))
        
        (org-e-caldav-element-update-buffer local-doc-updates local-doc)
        (org-e-caldav-set-state inbox-file state)

        (message "Synchronization of file \"%s\" complete." inbox-file)))))

(defun org-e-caldav-delete-event (uid)
  "Delete event with UID from calendar."
  (org-e-caldav-debug-print (format "Deleting event UID %s.\n" uid))
  (url-dav-delete-file (concat org-e-caldav-url uid ".ics")))

(defun org-e-caldav-show-conflicts (conflicts)
  "Show conflicts CONFLICTS, which looks like
   ((file1 lev1 . rev1) (file2 lev2 . rev2) ..)
in conflict window."
  (when conflicts
    (let ((buf (get-buffer-create org-e-caldav-conflicts-buffer)))
      (with-help-window buf
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (insert "There were some conflicts while merging. Here
are links to the problematic items. Edit the files to contain,
what you want to keep and sync again. The :sync property tells
you their origin.\n\n")
          
          (loop for (file . cfs) in conflicts do
                (loop for (lev . rev) in cfs do
                      (insert (format "- %s (in [[file:%s::*%s][%s]])\n"
                                      (plist-get lev :summary) file
                                      (plist-get lev :summary) file)
                              "\n"))))))))

(defun org-e-caldav-sync ()
  (interactive)
  (org-e-caldav-debug-print "Starting sync.\n")
  (org-e-caldav-check-connection)
  (org-e-caldav-load-state)
  (let* ((files (adjoin org-e-caldav-inbox org-e-caldav-files))
         (updated (make-hash-table :test 'equal))
         (updated-append (lambda (x) (mapc (lambda (y) (puthash y t updated)) x)))
         (filter-updated (lambda (x) (loop for y in x unless (gethash y updated) collect y)))
         deleted
         (delete-add (lambda (x) (push x deleted)))
         (conflicts
          (delq nil (mapcar (lambda (file)
                              (let ((cfs (org-e-caldav-sync-file
                                          file updated-append delete-add)))
                                (when cfs (cons file cfs))))
                            files))))

    (mapc 'org-e-caldav-delete-event (funcall filter-updated deleted))
    (org-e-caldav-sync-new-remotes org-e-caldav-inbox filter-updated)

    (org-e-caldav-show-conflicts conflicts))
  (org-e-caldav-write-state)
  (org-e-caldav-debug-print "Finished sync.\n"))

(provide 'org-e-caldav)

;;; org-e-caldav.el ends here
