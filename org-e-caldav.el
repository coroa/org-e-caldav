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
;; - Set `org-e-caldav-url' to the base address of your CalDAV server:
;;    * Owncloud: https://OWNCLOUD-SERVER-URL/remote.php/caldav/calendars/USERID
;;    * Google: https://www.google.com/calendar/dav
;;
;; - Set `org-e-caldav-calendar-id' to the calendar-id of your new calendar:
;;    * OwnCloud: Simply the name of the calendar.
;;    * Google: Click on 'calendar settings' and the id will be shown
;;      next to "Calendar Address". It is of the form
;;      ID@group.calendar.google.com. Do *not* omit the domain.
;;
;; - Set `org-e-caldav-files' to the list of org files you would like to
;;   sync. For everything else, you can use the org-e-icalendar-*
;;   variables, since org-e-caldav uses that package to generate the
;;   events.
;;
;; - Set `org-e-caldav-inbox' to an org filename where new entries from
;;   the calendar should be stored.
;;
;; Call `org-e-caldav-sync' to start the sync. The URL package will ask
;; you for username/password for accessing the calendar.


;;; Code:

(require 'url-dav)
(require 'icalendar)
(require 'org-e-icalendar)

(defvar org-e-caldav-url "https://www.google.com/calendar/dav"
  "Base URL for CalDAV access.")

(defvar org-e-caldav-calendar-id "abcde1234@group.calendar.google.com"
  "ID of your calendar.")

(defvar org-e-caldav-files '("~/org/appointments.org")
  "List of files which should end up in calendar.
Do NOT put `org-e-caldav-inbox' in here or you'll get duplicate
entries.")

(defvar org-e-caldav-inbox "~/org/from-calendar.org"
  "Filename for putting new entries obtained from calendar.")

(defvar org-e-caldav-debug t)
(defvar org-e-caldav-debug-buffer "*org-e-caldav-debug*")

;; Variables users usually should not have to touch

(defvar org-e-caldav-uid-property "CALDAVUID"
  "Property name in which to save the associated CAL ")

;; Internal cache variables

;;; The following cache definitions and functions come literally from
;;; os.el, written by Aurélien Aptel

(defvar org-e-caldav-cache-file (concat user-emacs-directory "org-e-caldav-cache")
  "Path to org-e-caldav cache file.")

(defvar org-e-caldav-cache-alist nil)

(defun org-e-caldav-set-cache (file eventlist)
  "Update FILE to EVENTLIST in `org-e-caldav-cache-alist'."
  (let ((cell (assoc file org-e-caldav-cache-alist)))
    (if cell
        (setcdr cell eventlist)
      (push (cons file eventlist) org-e-caldav-cache-alist))))

(defun org-e-caldav-get-cache (file)
  "Return the EVENTLIST for FILE in cache or nil."
    (cdr (assoc file org-e-caldav-cache-alist)))

(defun org-e-caldav-write-cache ()
  "Write org-e-caldav cache to `org-e-caldav-cache-file'."
  (with-temp-file org-e-caldav-cache-file
    (prin1 `(setq org-e-caldav-cache-alist ',org-e-caldav-cache-alist)
           (current-buffer))))

(defun org-e-caldav-load-cache ()
  "Load org-e-caldav cache from `org-e-caldav-cache-file'."
  (load org-e-caldav-cache-file 'noerror nil))

;;; A few functions from org-caldav.el by David Engster

(defun org-e-caldav-events-url ()
  "Return URL for events."
  (if (string-match "google\\.com" org-e-caldav-url)
      (concat org-e-caldav-url "/" org-e-caldav-calendar-id "/events/")
    (concat org-e-caldav-url "/" org-e-caldav-calendar-id "/")))

(defun org-e-caldav-check-connection ()
  "Check connection by doing a PROPFIND on CalDAV URL."
  (org-e-caldav-debug-print (format "Check connection - doing a PROPFIND on %s."
				  (org-e-caldav-events-url)))
  (let ((output (url-dav-request (org-e-caldav-events-url) "PROPFIND" nil nil 1)))
  (unless (eq (plist-get (cdar output) 'DAV:status) 200)
    (org-e-caldav-debug-print "Got error status from PROPFIND: " output)
    (error "Could not query CalDAV URL %s." (org-e-caldav-events-url))))
  t)

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
	 (description (icalendar--convert-string-for-import
		       (or (icalendar--get-event-property e 'DESCRIPTION)
			   "")))
	 (rrule (icalendar--get-event-property e 'RRULE))
	 (rdate (icalendar--get-event-property e 'RDATE))
	 (duration (icalendar--get-event-property e 'DURATION))
         start-t end-t range timestamp)
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
            '(:hour-end (nth 2 dtend-dec)
                        :minute-end (nth 1 dtend-dec)))
          dtend-dec (if end-t dtend-dec dtend-1-dec)
          range (not (equal dtstart-dec dtend-dec))
          timestamp
          `(timestamp
            :type ,(if range 'active-range 'active)
            :year-start ,(nth 5 dtstart-dec)
            :month-start ,(nth 4 dtstart-dec)
            :day-start ,(nth 3 dtstart-dec)
            ,@start-t
            :year-end ,(nth 5 dtend-dec)
            :month-end ,(nth 4 dtend-dec)
            :day-end ,(nth 3 dtend-dec)
            ,@end-t))                   ; XXX add repeater stuff from
                                        ; rrule and rdate

      `(:uid ,uid
             :timestamp ,timestamp
             :summary ,summary
             :description ,description)))

(defun org-e-caldav-event-to-ical (event)
  "Generate and return the ical equivalent to the event structure
event."
  (org-e-icalendar--vevent (plist-get event :headline)
                           (plist-get event :timestamp)
                           (plist-get event :uid)
                           (plist-get event :summary)
                           nil
                           (plist-get event :description)
                           nil))

(defun org-e-caldav-fetch-event (uid cache)
  (when uid
    (let* ((last-update (plist-get cache :date-cache))
           (url-request-extra-headers
            (when last-update
              `(("If-Modified-Since" . ,(url-get-normalized-date last-update))))))
      
      (with-current-buffer (url-retrieve-synchronously
                            (concat (org-e-caldav-events-url) (concat uid ".ics")))
        
        (case url-http-response-status
          (301 (assoc uid (plist-get cache :events)))
          (200 (delete-region
                (point-min)
                (progn (goto-char url-http-end-of-headers)
                       (search-forward "BEGIN:VCALENDAR")
                       (match-beginning 0)))
               (cons uid (org-e-caldav-ical-to-event uid)))
          (404 (cons uid nil))
          (t   (error "Couldn't retrieve event")))))))

(defun org-e-caldav-fetch-eventlist ()
  "Fetch list of events from remote calendar."
  (mapcar 'file-name-sans-extension
          (url-dav-directory-files (org-e-caldav-events-url) nil "\\.ics$")))

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
                        ,(mapcar
                          (lambda (x) `(node-property (:key ,(car x) :value ,(cdr x))))
                          alist))
    (let* ((drawer (org-e-caldav-find-property-drawer elem))
           (added (make-hash-table)))
      (org-element-map drawer
                       'node-property
                       (lambda (x) (let ((y (assoc (org-element-property :key x) alist)))
                                (when y
                                  (org-element-put-property x :value (cdr y))
                                  (puthash (car y) t added)))))

      (org-element-adopt-elements drawer
                                  (loop for (key . val) in alist
                                        unless (gethash key added)
                                        collect `(node-property (:key ,key :value ,val)))))))

(defun org-e-caldav-event-to-headline (event &optional headline)
  "Create a new headline or update an existing one. Returns the
innermost changed org-element structure."
  (let ((drawer-alist `((,org-e-caldav-uid-property . ,(plist-get event :uid))
                        (sync . ,(plist-get event :sync))))
        (summary (plist-get event :summary))
        (description (plist-get event :description))
        (timestamp (plist-get event :timestamp)))
    
    (if (null headline)
        `((headline (:title ,summary)
                    ((section nil                            
                              (,(org-e-caldav-alist-to-property-drawer drawer-alist)
                               ,timestamp
                               ,description)))))
      
      ;; XXX how do we update the description???
      ;; replace the first section completely by the the literal text
      ;; for now. hacky?
      (let ((drawer (org-e-caldav-find-property-drawer headline))
            (section (org-element-map headline 'section 'identity nil t 'headline)))
        (org-element-set-contents section
                                  (org-e-caldav-alist-to-property-drawer
                                   drawer-alist drawer)
                                  timestamp
                                  description)
        (if (not (equal summary (org-element-property :title headline)))
            (org-element-put-property headline :title summary)
          section)))))

(defun org-e-caldav-extract-description-from-headline (headline timestamp)
  "Return the interpreted first section, where the timestamp has
been temporarily removed."
  (let* ((section (car (org-element-contents headline)))
         (timestamp-cons (cl-member timestamp
                                    (org-element-contents
                                     (org-element-property :parent timestamp))))
         description)
    
    (setf (car timestamp-cons) ""
          description (org-element-interpret-data section)
          (car timestamp-cons) timestamp)
    
    description))

(defun org-e-caldav-strip-text-properties (text)
  "Remove the text properties connected to a string object"
  (set-text-properties 0 (length text) nil text)
  text)

(defun org-e-caldav-headline-to-event (headline timestamp)
  "Return event structure for a headline. An additional
property :headline contains the associated org-element structure."
  (let* ((prop-alist (org-e-caldav-property-drawer-to-alist headline))
         (uid (cdr (assoc :uid prop-alist))))
    (cons uid
          `(:uid ,uid
                 :timestamp ,timestamp
                 :summary ,(org-e-caldav-strip-text-properties
                            (org-element-property :title headline))
                 :description ,(org-e-caldav-extract-description-from-headline
                                headline timestamp)
                 :sync ,(cdr (assoc :sync prop-alist))
                 :headline ,headline))))

(defun org-element-closest (elem types fun)
  "Traverse upwards through its ancestors, stop at the first
match for which FUN doesn't return nil and return that value."
  (unless (listp types) (setq types (list types)))
  (let ((parent (org-element-property :parent elem)))
    (when parent
      (or (and (memq (org-element-type parent) types)
               (funcall fun parent))
          (org-element-closest parent types fun)))))

(defun org-element-update-buffer updates
  "Updates the current buffer according to the list updates,
which has the form '( elem1 elem2 elem3 ... ). Nested elements
are ignored.

Assumes the :begin :end properties of the elems correspond to the
buffer state.  XXX: overlays have to be treated
specifically. Refer to org-element-swap-A-B f.ex."
  (save-excursion
    (let ((sorted-by-beg (sort updates
                               (lambda (a b) (>= (org-element-property :begin a)
                                            (org-element-property :begin b)))))
          (sorted-by-end (sort updates
                               (lambda (a b) (>= (org-element-property :end a)
                                            (org-element-property :end b))))))

      (while sorted-by-end
        (let* ((elem (car sorted-by-end))
               (skip (+ (position elem sorted-by-beg) 1))
               (beg (org-element-property :begin a))
               (end (org-element-property :end a)))
                    
          (setq sorted-by-end (nthcdr skip sorted-by-end)
                sorted-by-beg (nthcdr skip sorted-by-beg))
          
          (goto-char beg)
          (delete-region beg end)
          (insert (org-element-interpret-data elem)))))))

(defun org-e-caldav-find-events (doc)
  "Find a list of events in the current buffer. An event is
represented by a headline containing an active timestamp."
  `(:events 
    ,(mapcar (lambda (ts) (org-e-caldav-headline-to-event
                      (org-element-closest ts 'headline 'identity) ts))
             (org-element-map doc
                              'timestamp
                              (lambda (x)
                                (when (memq (org-element-property :type x)
                                            '(active active-range)) x))))))

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
                     :minute-end)       ; XXX add repeater stuff
        unless (equal (plist-get a key)
                      (plist-get b key))
        return t))

;;; practically literally from os.el by Aurélien Aptel

(defun org-e-caldav-event-diff (a b)
  "Return an alist of properties that differ in A and B or nil if A = B.
The form of the alist is ((:property . (valueA valueB)...)"
  (let (diff)
    (append
     (dolist (key '(:summary
                    :description) diff)
       (let ((va (plist-get a key))
             (vb (plist-get b key)))
         (unless (equal va vb)
           (push `(,key . (,va ,vb)) diff))))
     (let ((ta (plist-get a :timestamp))
           (tb (plist-get b :timestamp)))
       (when (org-e-caldav-timestamp-diff ta tb)
         `((:timestamp . (,ta ,tb))))))))

(defun org-e-caldav-eventlist-diff (a b)
  "Return a diff eventlist which turns eventlist A to B when applied."
  (let* (diff
         (a-evs (plist-get a :events))
         (b-evs (plist-get b :events))
         (uid-list
          (append
           (loop for (uid . ev) in a-evs collect uid)
           (loop for (uid . ev) in b-evs
                 if uid collect uid))))
    (delete-dups uid-list)

    (append
     (dolist (uid uid-list diff)
       (let ((aev (cdr (assoc uid a-evs)))
             (bev (cdr (assoc uid b-evs))))
         (when (or (null aev)
                   (null bev)
                   (org-e-caldav-event-diff aev bev))
           (push (cons uid bev) diff))))
     (loop for (uid . ev) in b-evs
           if (null uid) collect (cons nil ev)))))

(defun org-e-caldav-eventlist-dups (eventlist)
  "Return non-nil if EVENTLIST contains events with the same UID.
The value returned is a list of duplicated ids."
  (let ((hash (make-hash-table))
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

(defun org-e-caldav-prepare-merge (local remote)
  "From LOCAL diff and REMOTE diff compute the necessary local
and remote changes and sort them into a plist with the
structure (:news (ev1 ..)
           :local-updates (..)
           :remote-updates (..)
           :remote-deletes (uid1 ..)
           :conflicts (..))
where the ev are normal events."
  (let ((added (make-hash-table))
        news lups rups rdels conflicts)

    ;; add all remote bugs
    (loop for (uid . lev) in local do
          (let* ((rpair (assoc uid remote))
                 (rev (cdr rpair)))

            ;; if there's a local event with the same uid, we have a
            ;; conflict

            ;; if the local event has a sync prop, it was merged by
            ;; the user, so we keep the local one (which might be the
            ;; remote from a previous sync)

            (cond
             ((null uid)
              (push lev news))
             ((and rpair
                   (null (plist-get lev :sync))
                   (os-event-diff lev rev))
              (push (cons (plist-put (copy-sequence lev) :sync 'conflict-local)
                          (plist-put (copy-sequence rev) :sync 'conflict-remote)) conflicts))
             ((null lev)
              (push uid rdels))
             (t
              (push lev rups)))
            
            ;; mark it
            (puthash uid t added)))

    ;; add changed remote events which are the unmarked events in local
    (loop for (uid . rev) in remote do
          (if (and uid (not (gethash uid added)))
              (push (or rev `(:uid ,uid :delete t)) lups)))

    `(:news ,news
           :local-updates ,lups
           :remote-updates ,rups
           :remote-deletes ,rdels
           :conflicts ,conflicts)))


(defun org-e-caldav-sync-file (file filter filter-append delete-append)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (let* ((local-doc (org-element-parse-buffer))
             local-doc-updates
             (local-doc-updates-add (lambda (x) (push x local-doc-updates)))
             (local (org-e-caldav-find-events local-doc)))
        
        (when (org-e-caldav-eventlist-dups local)
          (error "Sync file \"%s\" contains unmerged events." file))

        ;; local          cache          remote
        ;;    \          /    \          /
        ;;    parse    load   load     fetch
        ;;      \      /        \      /
        ;;     local-diff       remote-diff
        ;;             \        /
        ;;              \      /
        ;;             merged-diff --------send-------->
        ;;                                              (...)
        ;;               local   <--recv-updated-diff---
        ;;                 v
        ;;              merged
        ;;                 v
        ;;        new cache/local/remote

        (let* ((cache (org-e-caldav-get-cache file))
               (remote `(:events
                         ,(delq nil
                                (mapcar (lambda (x) (org-e-caldav-fetch-event (car x) cache))
                                        (if (equal file org-e-caldav-inbox)
                                            (funcall filter (org-e-caldav-fetch-eventlist))
                                          (plist-get local :events))))))
               (local-diff (org-e-caldav-eventlist-diff cache local))
               (remote-diff (org-e-caldav-eventlist-diff cache remote))
               (merge (org-e-caldav-prepare-merge local-diff remote-diff))
               (conflicts (plist-get merge :conflicts))
               (inbox (when (equal file org-e-caldav-inbox) local-doc)))

          (if conflicts (throw 'conflict conflicts))

          (funcall filter-append
                   (mapcar 'org-e-caldav-merge-remote (plist-get merge :remote-updates))
                   (mapcar (lambda (x) (org-e-caldav-merge-local x local local-doc-updates-add inbox))
                           (plist-get merge :local-updates))
                   (mapcar (lambda (x) (org-e-caldav-merge-new-remote x local-doc-updates-add))
                           (plist-get merge :news)))
          (funcall delete-append
                   (plist-get merge :remote-deletes)))

        (loop for pair in (plist-get local :events)
              if (null (car pair))
              do (setcar pair (plist-get (cdr pair) :uid)))
        (org-e-caldav-set-cache file (plist-put local :date-cache (current-time)))
        (org-element-update-buffer local-doc-updates)
        (message "Synchronization of file \"%s\" complete." file)))))

(defun org-e-caldav-sync ()
  (interactive)
  (let ((updated (make-hash-table))
        (filter-append (lambda (&rest x) (mapc (lambda (y) (puthash y t updated)) x)))
        (filter (lambda (x) (delq nil (mapcar (lambda (y) (unless (gethash y updated) y)) x))))
        deleted
        (delete-append (lambda (&rest x) (mapc (lambda (y) (setq deleted (append y deleted))) x)))
        conflicts)
    (mapc (lambda (file)
            (unless (equal file org-e-caldav-inbox)
              (let ((confs (catch 'conflict
                             (org-e-caldav-sync-file file filter filter-append delete-append)
                             nil)))
                (when confs (push (cons file confs) conflicts)))))
          org-e-caldav-files))

  (org-e-caldav-sync-file org-e-caldav-inbox filter filter-append delete-append)
  (mapc 'org-e-caldav-delete-event (funcall filter deleted))

  (org-e-caldav-show-conflicts conflicts))


(defun org-e-caldav-merge-remote (event)
  "Write a local change to a remote resource."
  (let ((uid (plist-get event :uid)))
    (url-dav-save-resource
     (concat (org-e-caldav-events-url) uid ".ics")
     (encode-coding-string (org-e-caldav-event-to-ical event) 'utf-8) "text/calendar; charset=UTF-8")
    uid))

(defun org-e-caldav-merge-local (event local local-doc-updates-add &optional inbox)
  "Update the local-doc org-element tree to reflect the remote
changes."
  (let* ((uid (plist-get event :uid))
         (lpair (assoc uid (plist-get local :events)))
         (lev (cdr lpair))
         (headline (plist-get lev :headline)))
    
    (local-doc-updates-add
     (if (null lev)
         (org-element-adopt-elements inbox
                                     (org-e-caldav-event-to-headline event))
       (org-e-caldav-event-to-headline event headline)))

    ;; as event was part of remote we need to update the local ones
    (setcdr lpair event)
    uid))

(defun org-e-caldav-merge-new-remote (event local-doc-updates-add)
  (let* ((headline (plist-get event :headline))
         (drawer (org-e-caldav-find-property-drawer headline))
         (uidlist (org-e-caldav-fetch-eventlist))
         (uid
          (loop for uid = (concat (format-time-string "%Y%m%dT%H%M%SZ" nil t)
                                  (format "-%d@%s" (random 10000) system-name))
                while (memq uid uidlist)
                finally return uid)))
    
    ;; event is an event from local, so that the side-effect change
    ;; effectively adds the uid also to an element in local
    (plist-put event :uid uid)
    (org-e-caldav-merge-remote event)
    
    (setq drawer (org-e-caldav-alist-to-property-drawer `(uid . ,uid) drawer))
    (funcall local-doc-updates-add
             (if (null (org-element-property :parent drawer))
                 (let ((section (or (org-element-map 'section 'identity nil t 'headline)
                                    headline)))
                   (org-element-set-contents
                    section
                    (cons (org-element-put-property drawer :parent section)
                          (org-element-get-contents section))))
               drawer))
    uid))

(defun org-e-caldav-delete-event (uid)
  "Delete event with UID from calendar."
  (org-e-caldav-debug-print (format "Deleting event UID %s.\n" uid))
  (url-dav-delete-file (concat (org-e-caldav-events-url) uid ".ics")))

(defun org-e-caldav-debug-print (&rest objects)
  "Print OBJECTS into debug buffer if `org-e-caldav-debug' is non-nil."
  (when org-e-caldav-debug
    (with-current-buffer (get-buffer-create org-e-caldav-debug-buffer)
      (dolist (cur objects)
	(if (stringp cur)
	    (insert cur)
	  (prin1 cur (current-buffer)))
	(insert "\n")))))

(provide 'org-e-caldav)

;;; org-e-caldav.el ends here
